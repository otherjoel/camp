#lang racket/base

;; Theme import for the built-in editor: parses TextMate .tmTheme and VS Code
;; JSON themes into a plain readable scheme datum, and computes the framework
;; color-scheme entry plan for a datum. Deliberately GUI-free.
;;
;; A scheme datum (design §4):
;;   (list name    ; string, unique — import replaces by name
;;         dark?   ; boolean
;;         tokens  ; (listof (list category hex bold? italic? underline?))
;;         globals); (listof (list category hex)), categories among
;;                 ; background foreground selection paren-match
;;
;; Token categories are framework's ten plus the markup-* set for punct's
;; Markdown structure; both are looked up as framework:syntax-color:scheme:<c>.

(require json
         racket/list
         racket/match
         racket/path
         racket/port
         racket/string
         xml/plist)

(provide import-theme-file
         read-tmtheme
         read-vscode-theme
         read-jsonc
         normalize-hex
         dark-background?
         scope-match?
         scheme-entry-plan
         scheme-entry-names
         scheme-name
         scheme-dark?
         scheme-tokens
         scheme-globals)

(define scheme-name first)
(define scheme-dark? second)
(define scheme-tokens third)
(define scheme-globals fourth)

;; ============================================================================
;; Colors

;; "#RGB", "#RRGGBB" or "#RRGGBBAA" in any case → "#rrggbb"; anything else #f
(define (normalize-hex v)
  (and (string? v)
       (let ([s (string-downcase v)])
         (cond
           [(regexp-match #px"^#([0-9a-f]{6})(?:[0-9a-f]{2})?$" s)
            => (λ (m) (string-append "#" (cadr m)))]
           [(regexp-match #px"^#([0-9a-f])([0-9a-f])([0-9a-f])$" s)
            => (λ (m) (apply string-append "#" (map (λ (c) (string-append c c))
                                                    (cdr m))))]
           [else #f]))))

(define (dark-background? hex)
  (define (channel i) (string->number (substring hex i (+ i 2)) 16))
  (< (/ (+ (* 0.299 (channel 1)) (* 0.587 (channel 3)) (* 0.114 (channel 5)))
        255)
     0.5))

;; ============================================================================
;; Scope matching (design §4)

(define category-representatives
  '((symbol "variable.other" "variable" "entity.name.function"
            "support.function" "entity.name")
    (keyword "keyword.control" "keyword" "storage.type" "storage")
    (comment "comment")
    (string "string")
    (constant "constant.numeric" "constant.language" "constant.character"
              "constant")
    (hash-colon-keyword "keyword.other" "constant.other" "entity.name.tag"
                        "support.type.property-name")
    (parenthesis "punctuation.section" "meta.brace" "punctuation")
    (error "invalid.illegal" "invalid")
    ;; punct's Markdown structure, arriving as the lexer's 'markup attribute
    (markup-heading "markup.heading" "entity.name.section")
    (markup-emphasis "markup.italic")
    (markup-strong "markup.bold")
    (markup-code "markup.inline.raw" "markup.raw" "markup.fenced_code")
    (markup-link "markup.underline.link" "string.other.link" "markup.link")
    (markup-list "markup.list" "punctuation.definition.list")
    (markup-quote "markup.quote")
    (markup-rule "meta.separator" "markup.separator")
    (markup-meta "entity.name.tag" "support.type.property-name" "keyword.other")))

(define token-categories
  '(symbol keyword comment string text constant hash-colon-keyword
    parenthesis error other
    markup-heading markup-emphasis markup-strong markup-code markup-link
    markup-list markup-quote markup-rule markup-meta))

(define (scope-match? sel rep)
  (define (dot-prefix? a b)
    (and (string-prefix? b a)
         (or (= (string-length a) (string-length b))
             (char=? (string-ref b (string-length a)) #\.))))
  (or (dot-prefix? sel rep) (dot-prefix? rep sel)))

(struct rule (selectors color bold? italic? underline?))

;; Comma-split scope selectors; a descendant selector contributes its last
;; (target) element
(define (scope-field->selectors v)
  (define parts
    (cond [(string? v) (string-split v ",")]
          [(list? v) (append-map (λ (s) (if (string? s) (string-split s ",") '()))
                                 v)]
          [else '()]))
  (filter-map (λ (s) (let ([ws (string-split s)]) (and (pair? ws) (last ws))))
              parts))

;; The first representative with any matching rule decides the category; a
;; rule's strength is its longest matching selector
(define (match-category cat rules)
  (for/or ([rep (in-list (cdr (assq cat category-representatives)))])
    (define-values (best best-len)
      (for/fold ([best #f] [best-len 0]) ([r (in-list rules)])
        (define len
          (for/fold ([len 0]) ([sel (in-list (rule-selectors r))]
                               #:when (scope-match? sel rep))
            (max len (string-length sel))))
        (if (> len best-len) (values r len) (values best best-len))))
    (and best
         (list (rule-color best) (rule-bold? best) (rule-italic? best)
               (rule-underline? best)))))

;; text and other always take the global foreground; categories with no
;; matching rule are omitted so built-in colors show through
(define (rules->tokens rules fg)
  (for*/list ([cat (in-list token-categories)]
              [tok (in-value (if (memq cat '(text other))
                                 (list fg #f #f #f)
                                 (match-category cat rules)))]
              #:when tok)
    (cons cat tok)))

;; ============================================================================
;; Shared normalization

(define (font-style-flags v)
  (define styles (if (string? v) (string-split v) '()))
  (values (and (member "bold" styles) #t)
          (and (member "italic" styles) #t)
          (and (member "underline" styles) #t)))

(define (globals-list bg fg selection paren-match)
  (for/list ([g (in-list `((background ,bg)
                           (foreground ,fg)
                           (selection ,selection)
                           (paren-match ,(or paren-match selection))))]
             #:when (second g))
    g))

(define (require-colors who name bg fg)
  (unless (and bg fg)
    (error who "theme ~s has no usable background/foreground colors" name)))

;; ============================================================================
;; TextMate .tmTheme

(define (plist-ref d key)
  (match d
    [(cons 'dict pairs)
     (for/or ([p (in-list pairs)])
       (match p
         [(list 'assoc-pair (== key) v) v]
         [_ #f]))]
    [_ #f]))

(define (read-tmtheme in [fallback-name "Imported theme"])
  (define top
    (with-handlers ([exn:fail? (λ (e) (error 'read-tmtheme
                                             "malformed .tmTheme plist: ~a"
                                             (exn-message e)))])
      (read-plist in)))
  (define name (let ([n (plist-ref top "name")]) (if (string? n) n fallback-name)))
  (define entries (match (plist-ref top "settings") [(cons 'array es) es] [_ '()]))
  (define-values (unscoped scoped)
    (partition (λ (e) (not (plist-ref e "scope"))) entries))
  (define globals (and (pair? unscoped) (plist-ref (first unscoped) "settings")))
  (define (global key) (normalize-hex (plist-ref globals key)))
  (define bg (global "background"))
  (define fg (global "foreground"))
  (require-colors 'read-tmtheme name bg fg)
  (define rules
    (for*/list ([e (in-list scoped)]
                [settings (in-value (plist-ref e "settings"))]
                [color (in-value (normalize-hex (plist-ref settings "foreground")))]
                #:when color)
      (define-values (b i u) (font-style-flags (plist-ref settings "fontStyle")))
      (rule (scope-field->selectors (plist-ref e "scope")) color b i u)))
  (list name (dark-background? bg)
        (rules->tokens rules fg)
        (globals-list bg fg (global "selection") #f)))

;; ============================================================================
;; VS Code theme JSON

;; VS Code theme files are JSONC: tolerate // and /* */ comments and trailing
;; commas, none of which read-json accepts. Both passes track strings so
;; comment markers, braces, and commas inside them survive.
(define (read-jsonc in)
  (read-json (open-input-string
              (strip-trailing-commas (strip-comments (port->string in))))))

(define (strip-comments s)
  (define n (string-length s))
  (define out (open-output-string))
  (let loop ([i 0] [state 'code])
    (when (< i n)
      (define c (string-ref s i))
      (define (next? ch) (and (< (add1 i) n) (char=? (string-ref s (add1 i)) ch)))
      (case state
        [(code)
         (cond
           [(and (char=? c #\/) (next? #\/)) (loop (+ i 2) 'line-comment)]
           [(and (char=? c #\/) (next? #\*)) (loop (+ i 2) 'block-comment)]
           [else (write-char c out)
                 (loop (add1 i) (if (char=? c #\") 'in-string 'code))])]
        [(in-string)
         (write-char c out)
         (loop (add1 i) (cond [(char=? c #\\) 'escape]
                              [(char=? c #\") 'code]
                              [else 'in-string]))]
        [(escape) (write-char c out) (loop (add1 i) 'in-string)]
        [(line-comment)
         (when (char=? c #\newline) (write-char c out))
         (loop (add1 i) (if (char=? c #\newline) 'code 'line-comment))]
        [(block-comment)
         (cond
           [(and (char=? c #\*) (next? #\/))
            (write-char #\space out)
            (loop (+ i 2) 'code)]
           [else (loop (add1 i) 'block-comment)])])))
  (get-output-string out))

(define (strip-trailing-commas s)
  (define n (string-length s))
  (define out (open-output-string))
  (let loop ([i 0] [state 'code])
    (when (< i n)
      (define c (string-ref s i))
      (case state
        [(code)
         (cond
           [(char=? c #\,)
            (define j (let skip ([j (add1 i)])
                        (if (and (< j n) (char-whitespace? (string-ref s j)))
                            (skip (add1 j))
                            j)))
            (unless (and (< j n) (memv (string-ref s j) '(#\} #\])))
              (write-char c out))
            (loop (add1 i) 'code)]
           [else (write-char c out)
                 (loop (add1 i) (if (char=? c #\") 'in-string 'code))])]
        [(in-string)
         (write-char c out)
         (loop (add1 i) (cond [(char=? c #\\) 'escape]
                              [(char=? c #\") 'code]
                              [else 'in-string]))]
        [(escape) (write-char c out) (loop (add1 i) 'in-string)])))
  (get-output-string out))

(define (read-vscode-theme in [fallback-name "Imported theme"])
  (define j
    (with-handlers ([exn:fail? (λ (e) (error 'read-vscode-theme
                                             "malformed theme JSON: ~a"
                                             (exn-message e)))])
      (read-jsonc in)))
  (unless (hash? j)
    (error 'read-vscode-theme "expected a JSON object at the top level"))
  (when (hash-has-key? j 'include)
    (error 'read-vscode-theme
           "themes based on \"include\" are not supported; import the included base theme instead"))
  (define name (let ([n (hash-ref j 'name #f)]) (if (string? n) n fallback-name)))
  (define colors (let ([c (hash-ref j 'colors #f)]) (if (hash? c) c #hasheq())))
  (define (color key) (normalize-hex (hash-ref colors key #f)))
  (define bg (color '|editor.background|))
  (define fg (color '|editor.foreground|))
  (require-colors 'read-vscode-theme name bg fg)
  (define rules
    (for*/list ([e (in-list (let ([tc (hash-ref j 'tokenColors #f)])
                              (if (list? tc) tc '())))]
                #:when (hash? e)
                [settings (in-value (hash-ref e 'settings #f))]
                #:when (hash? settings)
                [color (in-value (normalize-hex (hash-ref settings 'foreground #f)))]
                #:when color)
      (define-values (b i u) (font-style-flags (hash-ref settings 'fontStyle #f)))
      (rule (scope-field->selectors (hash-ref e 'scope '())) color b i u)))
  (define dark?
    (match (hash-ref j 'type #f)
      ["dark" #t]
      ["light" #f]
      [_ (dark-background? bg)]))
  (list name dark? (rules->tokens rules fg)
        (globals-list bg fg
                      (color '|editor.selectionBackground|)
                      (color '|editorBracketMatch.background|))))

;; ============================================================================
;; Import

(define (import-theme-file path)
  (define p (if (path? path) path (string->path path)))
  (define stem
    (let ([f (file-name-from-path p)])
      (if f (path->string (path-replace-extension f #"")) "Imported theme")))
  (define ext (let ([e (path-get-extension p)])
                (and e (string-downcase (bytes->string/utf-8 e)))))
  (define reader
    (case ext
      [(".tmtheme") read-tmtheme]
      [(".json") read-vscode-theme]
      [else (error 'import-theme-file
                   "expected a .tmTheme or VS Code theme .json file: ~a" p)]))
  (call-with-input-file p (λ (in) (reader in stem))))

;; ============================================================================
;; Entry plan (design §3)

(define global-entry-names
  '((background framework:basic-canvas-background)
    (foreground framework:default-text-color)
    (selection camp:vim-selection-color)
    (paren-match framework:paren-match-color)))

(define (category->entry-name cat)
  (string->symbol (format "framework:syntax-color:scheme:~a" cat)))

;; Every entry a scheme can touch; entries absent from a scheme's plan must
;; have camp's adjustment slot removed so built-in colors show through
(define scheme-entry-names
  (append (map category->entry-name token-categories)
          (map second global-entry-names)))

;; datum → (listof (list entry-name polarity value-spec)): value-spec is
;; (hex bold? italic? underline?) for style-delta entries and a bare hex
;; string for color entries. Globals with no entry (a stored datum's caret,
;; from before the caret stopped being themed) are skipped.
(define (scheme-entry-plan scheme)
  (define polarity (if (scheme-dark? scheme) 'dark 'light))
  (append
   (for/list ([t (in-list (scheme-tokens scheme))])
     (list (category->entry-name (first t)) polarity (rest t)))
   (for*/list ([g (in-list (scheme-globals scheme))]
               [entry (in-value (assq (first g) global-entry-names))]
               #:when entry)
     (list (second entry) polarity (second g)))))
