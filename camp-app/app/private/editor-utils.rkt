#lang racket/base

;; Pure helpers for the in-app editor

(require racket/list
         racket/math
         racket/string)

(provide auto-fill-edits
         editor-title
         fill-paragraph
         fill-unit
         font-slot-label
         gutter-font-size
         markup-aware
         next-font-slot
         position-after-edits
         promote-markup
         saved-message
         use-internal-editor?)

(define (editor-title path dirty?)
  (define parts (explode-path path))
  (define folder
    (and (> (length parts) 1)
         (let ([f (list-ref parts (- (length parts) 2))])
           (and (path? f) (relative-path? f) (path->string f)))))
  (string-append (if dirty? "• " "")
                 (path->string (last parts))
                 (if folder (string-append " — " folder) "")))

(define (use-internal-editor? editor-pref)
  (not (non-empty-string? editor-pref)))

;; ============================================================================
;; Markup coloring
;;
;; Punct's lexer tags Markdown structure with a 'markup attribute beside a
;; standard 'type. Framework's colorer looks styles up by 'color when present,
;; so promoting 'markup x to 'color markup-x routes those tokens to the
;; markup-* scheme entries (appearance.rkt) while 'type keeps its meaning for
;; spell-checking and navigation.

(define (promote-markup attribs)
  (define m (and (hash? attribs) (hash-ref attribs 'markup #f)))
  (if m
      (hash-set attribs 'color (string->symbol (format "markup-~a" m)))
      attribs))

(define (markup-aware get-token)
  (λ (in offset mode)
    (define-values (lexeme attribs paren start end backup new-mode)
      (get-token in offset mode))
    (values lexeme (promote-markup attribs) paren start end backup new-mode)))

;; Vim's post-write report ("8L, 246B written") plus a timestamp
(define (saved-message text bytes stamp)
  (format "Saved: ~aL, ~aB written • ~a"
          (for/sum ([_ (in-lines (open-input-string text))]) 1)
          bytes stamp))

;; ============================================================================
;; Font slots
;;
;; A slot is (face size); #f in either position means the framework default,
;; resolved at apply time in camp/app/private/fonts.

(define (next-font-slot i)
  (modulo (add1 i) 3))

(define (gutter-font-size size)
  (max 7 (exact-round (* 0.7 size))))

(define (font-slot-label idx slot)
  (define face (first slot))
  (define size (second slot))
  (format "Slot ~a: ~a" (add1 idx)
          (cond
            [(and (not face) (not size)) "(default)"]
            [size (format "~a ~a" (or face "(default)") size)]
            [else face])))

;; ============================================================================
;; Markdown-aware paragraph filling
;;
;; fill-unit finds the CommonMark "fill unit" around a line — a single list
;; item (with its marker preserved and a hanging indent), a blockquoted
;; paragraph (quote prefix on every line), or a plain paragraph — and refuses
;; lines where re-wrapping would corrupt structure (code, headings, metadata).

(define blank-rx   #px"^[ \t]*$")
(define lang-rx    #px"^#lang\\s")
(define fence-rx   #px"^ {0,3}(```|~~~)")
(define heading-rx #px"^ {0,3}#{1,6}(\\s|$)")
(define hr-rx      #px"^ {0,3}(?:(?:-[ \t]*){3,}|(?:\\*[ \t]*){3,}|(?:_[ \t]*){3,})$")
(define setext-rx  #px"^ {0,3}=+[ \t]*$")
(define table-rx   #px"^ {0,3}\\|")
(define quote-rx   #px"^[ \t]{0,3}(?:>[ \t]?)+")
(define item-rx    #px"^([ \t]*)([-*+]|[0-9]{1,9}[.)])[ \t]+(?=\\S)")

(define (blank-line? l) (regexp-match? blank-rx l))

(define (quote-prefix l)
  (cond [(regexp-match quote-rx l) => car]
        [else ""]))

(define (quote-depth l)
  (for/sum ([c (in-string (quote-prefix l))] #:when (char=? c #\>)) 1))

(define (strip-quote l)
  (substring l (string-length (quote-prefix l))))

(define (boundary-line? l)
  (define body (strip-quote l))
  (for/or ([rx (in-list (list blank-rx lang-rx fence-rx heading-rx hr-rx setext-rx table-rx))])
    (regexp-match? rx body)))

(define (item-prefix l)
  (cond [(regexp-match item-rx l) => car]
        [else #f]))

(define (in-fence? lines idx)
  (odd? (for/sum ([i (in-range idx)]
                  #:when (regexp-match? fence-rx (vector-ref lines i)))
          1)))

;; Index range of the document's leading metadata block, or #f
(define (metadata-bounds lines)
  (define n (vector-length lines))
  (define open
    (for/first ([i (in-range n)]
                #:unless (or (blank-line? (vector-ref lines i))
                             (regexp-match? lang-rx (vector-ref lines i))))
      (and (regexp-match? hr-rx (vector-ref lines i)) i)))
  (and open
       (for/first ([i (in-range (add1 open) n)]
                   #:when (regexp-match? hr-rx (vector-ref lines i)))
         (cons open i))))

(define (wrap-words words width first-prefix cont-prefix)
  (string-join
   (for/fold ([lines '()]
              [line (string-append first-prefix (car words))]
              #:result (reverse (cons line lines)))
             ([w (in-list (cdr words))])
     (define joined (string-append line " " w))
     (if (<= (string-length joined) width)
         (values lines joined)
         (values (cons line lines) (string-append cont-prefix w))))
   "\n"))

;; Reflow prose to width, greedily; the first line's indent is applied to
;; every line and words are never broken
(define (fill-paragraph text width)
  (define indent (car (regexp-match #px"^[ \t]*" text)))
  (define words (string-split text))
  (if (null? words)
      text
      (wrap-words words width indent indent)))

;; Prefixes for a unit's first and later lines: quote markers and indent
;; carry over, a list marker becomes a hanging indent
(define (fill-prefixes l)
  (define qp (quote-prefix l))
  (define body (strip-quote l))
  (define lead (or (item-prefix body) (car (regexp-match #px"^[ \t]*" body))))
  (values (string-append qp lead)
          (string-append qp (regexp-replace* #px"\\S" lead " "))))

(define (rewrap lines top bottom width)
  (define-values (first-prefix cont-prefix) (fill-prefixes (vector-ref lines top)))
  (wrap-words (append-map
               string-split
               (cons (substring (vector-ref lines top) (string-length first-prefix))
                     (for/list ([i (in-range (add1 top) (add1 bottom))])
                       (strip-quote (vector-ref lines i)))))
              width first-prefix cont-prefix))

(define (fillable? lines idx)
  (and (< idx (vector-length lines))
       (not (boundary-line? (vector-ref lines idx)))
       (not (in-fence? lines idx))
       (not (let ([md (metadata-bounds lines)])
              (and md (< (car md) idx) (<= idx (cdr md)))))))

;; Last line of the fill unit holding line idx
(define (unit-bottom lines idx)
  (define depth (quote-depth (vector-ref lines idx)))
  (let loop ([i idx])
    (define next (add1 i))
    (if (or (= next (vector-length lines))
            (boundary-line? (vector-ref lines next))
            (not (= depth (quote-depth (vector-ref lines next))))
            (item-prefix (strip-quote (vector-ref lines next))))
        i
        (loop next))))

(define hard-break-rx #px"(?: {2,}|\\\\)$")

;; Auto-fill: the (start end replacement) edits, rightmost first, that hard-wrap
;; line idx as fill-unit would and carry what overflows onto the unit's next
;; line, and so on down until a line fits. Offsets count from the start of line
;; idx and keep the text's own spacing, so no line exceeds width. line-start is
;; the offset that lands in column 0 once the edits so far are made.
(define (auto-fill-edits lines idx width)
  (define (line i) (vector-ref lines i))
  (cond
    [(and (fillable? lines idx)
          (> (string-length (line idx)) width))
     (define-values (first-prefix cont-prefix) (fill-prefixes (line idx)))
     (define bottom (unit-bottom lines idx))
     (let loop ([i idx] [base 0] [skip (string-length first-prefix)]
                [edits '()] [line-start 0] [prev-end #f])
       (define-values (edits* line-start* prev-end* broke?)
         (for/fold ([edits edits] [line-start line-start] [prev-end prev-end] [broke? #f])
                   ([w (in-list (regexp-match-positions* #px"\\S+" (line i) skip))])
           (define-values (from to) (values (+ base (car w)) (+ base (cdr w))))
           (if (and prev-end (> (- to line-start) width))
               (values (cons (list prev-end from (string-append "\n" cont-prefix)) edits)
                       (- from (string-length cont-prefix))
                       to
                       #t)
               (values edits line-start to broke?))))
       (define next-base (+ base (string-length (line i)) 1))
       (define w
         (and broke?
              (< i bottom)
              (not (regexp-match? hard-break-rx (line i)))
              (let ([next (line (add1 i))])
                (regexp-match-positions #px"\\S+" next (string-length (quote-prefix next))))))
       (define joined-start (and w (+ line-start* (- (+ next-base (caar w)) prev-end* 1))))
       (if (and w (<= (- (+ next-base (cdar w)) joined-start) width))
           (loop (add1 i) next-base (caar w)
                 (cons (list prev-end* (+ next-base (caar w)) " ") edits*)
                 joined-start
                 prev-end*)
           edits*))]
    [else '()]))

;; Where a position lands once edits are made, staying put at an edit's start
(define (position-after-edits pos edits)
  (for/fold ([p pos]) ([e (in-list edits)] #:when (< (first e) pos))
    (+ p (string-length (third e)) (- (first e) (min pos (second e))))))

(define (fill-unit lines idx width)
  (define (line i) (vector-ref lines i))
  (and (fillable? lines idx)
       (let* ([depth (quote-depth (line idx))]
              [top
               (let loop ([i idx])
                 (cond
                   [(item-prefix (strip-quote (line i))) i]
                   [(or (zero? i)
                        (boundary-line? (line (sub1 i)))
                        (not (= depth (quote-depth (line (sub1 i))))))
                    i]
                   [(item-prefix (strip-quote (line (sub1 i)))) (sub1 i)]
                   [else (loop (sub1 i))]))]
              [bottom (unit-bottom lines idx)])
         (list top bottom (rewrap lines top bottom width)))))
