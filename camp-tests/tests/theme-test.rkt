#lang racket/base

;; Theme parsing and normalization (design §4), and the framework entry
;; application plan (design §3). Everything here is GUI-free.

(require json
         racket/port
         racket/runtime-path
         rackunit
         rackunit/text-ui
         camp/app/private/theme)

(define-runtime-path themes-dir "fixtures/themes")
(define (fixture name) (build-path themes-dir name))

;; Builds a synthetic VS Code theme; each rule is (scope fg) or (scope fg style)
(define (vs-theme #:type [type #f] #:bg [bg "#101010"] #:fg [fg "#F0F0F0"]
                  . rules)
  (define j
    (let ([base (hasheq 'name "Synthetic"
                        'colors (hasheq '|editor.background| bg
                                        '|editor.foreground| fg)
                        'tokenColors
                        (for/list ([r (in-list rules)])
                          (hasheq 'scope (car r)
                                  'settings
                                  (if (null? (cddr r))
                                      (hasheq 'foreground (cadr r))
                                      (hasheq 'foreground (cadr r)
                                              'fontStyle (caddr r))))))])
      (if type (hash-set base 'type type) base)))
  (read-vscode-theme (open-input-string (jsexpr->string j)) "synthetic"))

(define (token cat scheme) (assq cat (scheme-tokens scheme)))

(define (jsonc s) (read-jsonc (open-input-string s)))

(define theme-tests
  (test-suite
   "theme parsing"

   (test-suite
    "color normalization"
    (test-case "six-digit hex, case folded"
      (check-equal? (normalize-hex "#AABBCC") "#aabbcc"))
    (test-case "alpha suffix dropped"
      (check-equal? (normalize-hex "#44475A99") "#44475a"))
    (test-case "three-digit hex expanded"
      (check-equal? (normalize-hex "#abc") "#aabbcc"))
    (test-case "non-hex values rejected"
      (check-false (normalize-hex "red"))
      (check-false (normalize-hex "#12345"))
      (check-false (normalize-hex 42))
      (check-false (normalize-hex #f))))

   (test-suite
    "scope matching"
    (test-case "selector as dot-boundary prefix of representative"
      (check-true (scope-match? "keyword" "keyword.control")))
    (test-case "representative as dot-boundary prefix of selector"
      (check-true (scope-match? "keyword.control.import" "keyword.control")))
    (test-case "exact match"
      (check-true (scope-match? "comment" "comment")))
    (test-case "prefix must end on a dot boundary"
      (check-false (scope-match? "keywordx" "keyword.control"))
      (check-false (scope-match? "keyword.controls" "keyword.control")))
    (test-case "disjoint scopes do not match"
      (check-false (scope-match? "punctuation.definition" "punctuation.section")))

    (test-case "first matching representative wins over later exact matches"
      (define d (vs-theme (list "keyword" "#AA0000")
                          (list "storage.type" "#00BB00")))
      (check-equal? (token 'keyword d) '(keyword "#aa0000" #f #f #f)))
    (test-case "later representative used when earlier ones unmatched"
      (define d (vs-theme (list "storage" "#00BB00")))
      (check-equal? (token 'keyword d) '(keyword "#00bb00" #f #f #f)))
    (test-case "longest matching selector breaks ties"
      (define d (vs-theme (list "constant" "#111111")
                          (list "constant.numeric" "#222222")))
      (check-equal? (token 'constant d) '(constant "#222222" #f #f #f)))
    (test-case "descendant selectors match by their last element"
      (define d (vs-theme (list "meta.function-call punctuation" "#333333")))
      (check-equal? (token 'parenthesis d) '(parenthesis "#333333" #f #f #f)))
    (test-case "uncovered categories are omitted"
      (define d (vs-theme (list "comment" "#444444")))
      (check-false (token 'string d))
      (check-false (token 'error d))))

   (test-suite
    "markup categories"
    (test-case "Markdown scopes map to the markup categories"
      (define d (vs-theme (list "markup.heading" "#111111" "bold")
                          (list "markup.italic" "#222222" "italic")
                          (list "markup.bold" "#333333" "bold")
                          (list "markup.inline.raw" "#444444")
                          (list "markup.underline.link" "#555555" "underline")
                          (list "markup.list" "#666666")
                          (list "markup.quote" "#777777")
                          (list "meta.separator" "#888888")
                          (list "entity.name.tag" "#999999")))
      (check-equal? (token 'markup-heading d) '(markup-heading "#111111" #t #f #f))
      (check-equal? (token 'markup-emphasis d) '(markup-emphasis "#222222" #f #t #f))
      (check-equal? (token 'markup-strong d) '(markup-strong "#333333" #t #f #f))
      (check-equal? (token 'markup-code d) '(markup-code "#444444" #f #f #f))
      (check-equal? (token 'markup-link d) '(markup-link "#555555" #f #f #t))
      (check-equal? (token 'markup-list d) '(markup-list "#666666" #f #f #f))
      (check-equal? (token 'markup-quote d) '(markup-quote "#777777" #f #f #f))
      (check-equal? (token 'markup-rule d) '(markup-rule "#888888" #f #f #f))
      (check-equal? (token 'markup-meta d) '(markup-meta "#999999" #f #f #f)))
    (test-case "a bare markup rule covers every markup category except meta"
      (define d (vs-theme (list "markup" "#abcdef")))
      (for ([c (in-list '(markup-heading markup-emphasis markup-strong markup-code
                          markup-link markup-list markup-quote markup-rule))])
        (check-equal? (token c d) (list c "#abcdef" #f #f #f)))
      (check-false (token 'markup-meta d)))
    (test-case "fenced and block raw scopes reach markup-code"
      (check-equal? (token 'markup-code (vs-theme (list "markup.raw.block" "#010101")))
                    '(markup-code "#010101" #f #f #f))
      (check-equal? (token 'markup-code (vs-theme (list "markup.fenced_code.block" "#020202")))
                    '(markup-code "#020202" #f #f #f)))
    (test-case "uncovered markup categories are omitted"
      (check-false (token 'markup-heading (vs-theme (list "keyword" "#000000"))))))

   (test-suite
    "polarity"
    (test-case "explicit type wins over luminance"
      (check-false (scheme-dark? (vs-theme #:type "light" #:bg "#000000")))
      (check-true (scheme-dark? (vs-theme #:type "dark" #:bg "#FFFFFF"))))
    (test-case "luminance below 0.5 means dark"
      (check-true (scheme-dark? (vs-theme #:bg "#7F7F7F")))
      (check-false (scheme-dark? (vs-theme #:bg "#808080"))))
    (test-case "dark-background? matches the luminance formula"
      (check-true (dark-background? "#000000"))
      (check-false (dark-background? "#ffffff"))))

   (test-suite
    "JSONC reading"
    (test-case "line and block comments stripped, trailing commas dropped"
      (check-equal? (jsonc "{\"a\": 1, // c\n \"b\": [1, 2,],}")
                    (hasheq 'a 1 'b '(1 2))))
    (test-case "comment markers inside strings are preserved"
      (check-equal? (jsonc "{\"a\": \"http://u\" /* tail */}")
                    (hasheq 'a "http://u"))
      (check-equal? (jsonc "{\"a\": \"/* not a comment */\"}")
                    (hasheq 'a "/* not a comment */")))
    (test-case "braces and commas inside strings are preserved"
      (check-equal? (jsonc "{\"a\": \",}\", \"b\": 1}")
                    (hasheq 'a ",}" 'b 1)))
    (test-case "escaped quote before a trailing comma"
      (check-equal? (jsonc "{\"a\": \"s\\\\\",}")
                    (hasheq 'a "s\\"))))

   (test-suite
    "tmTheme parsing"
    (test-case "minimal theme"
      (check-equal?
       (import-theme-file (fixture "minimal.tmTheme"))
       '("Minimal Light"
         #f
         ((comment "#998877" #f #f #f)
          (text "#222222" #f #f #f)
          (other "#222222" #f #f #f))
         ((background "#ffffff")
          (foreground "#222222")))))
    (test-case "realistic theme"
      (check-equal?
       (import-theme-file (fixture "realistic.tmTheme"))
       '("Firelight"
         #t
         ((symbol "#ffb86c" #f #f #f)
          (keyword "#ff79c6" #t #f #f)
          (comment "#6272a4" #f #t #f)
          (string "#f1fa8c" #f #f #f)
          (text "#f8f8f2" #f #f #f)
          (constant "#bd93f9" #f #f #f)
          (hash-colon-keyword "#ff5555" #f #f #f)
          (parenthesis "#e9f284" #f #f #f)
          (error "#ff5555" #t #f #t)
          (other "#f8f8f2" #f #f #f)
          (markup-link "#f1fa8c" #f #f #f)
          (markup-list "#abb2bf" #f #f #f)
          (markup-meta "#ff5555" #f #f #f))
         ((background "#282a36")
          (foreground "#f8f8f2")
          (selection "#44475a")
          (paren-match "#44475a"))))))

   (test-suite
    "VS Code parsing"
    (test-case "dark JSONC theme"
      (check-equal?
       (import-theme-file (fixture "vscode-dark.json"))
       '("Nightfall // JSONC edition"
         #t
         ((symbol "#f38ba8" #f #f #f)
          (keyword "#cba6f7" #t #f #f)
          (comment "#6c7086" #f #t #f)
          (string "#a6e3a1" #f #f #f)
          (text "#cdd6f4" #f #f #f)
          (constant "#fab387" #f #f #f)
          (hash-colon-keyword "#cba6f7" #t #f #f)
          (parenthesis "#9399b2" #f #f #f)
          (error "#f38ba8" #f #f #t)
          (other "#cdd6f4" #f #f #f)
          (markup-link "#a6e3a1" #f #f #f)
          (markup-list "#9399b2" #f #f #f)
          (markup-meta "#cba6f7" #t #f #f))
         ((background "#1e1e2e")
          (foreground "#cdd6f4")
          (selection "#585b70")
          (paren-match "#3e4152")))))
    (test-case "name falls back to the supplied stem"
      (define d (read-vscode-theme
                 (open-input-string
                  "{\"colors\": {\"editor.background\": \"#111111\", \"editor.foreground\": \"#EEEEEE\"}}")
                 "stemmy"))
      (check-equal? (scheme-name d) "stemmy")))

   (test-suite
    "serialization"
    (test-case "datum round-trips through write and read"
      (define d (import-theme-file (fixture "realistic.tmTheme")))
      (check-equal? (with-input-from-string (format "~s" d) read) d)))

   (test-suite
    "errors"
    (test-case "broken plist raises"
      (check-exn exn:fail? (λ () (import-theme-file (fixture "broken.tmTheme")))))
    (test-case "theme without background/foreground raises"
      (check-exn #rx"background" (λ () (import-theme-file (fixture "no-colors.json")))))
    (test-case "include-based VS Code theme rejected with a clear message"
      (check-exn #rx"include" (λ () (import-theme-file (fixture "include.json")))))
    (test-case "unsupported extension rejected"
      (check-exn #rx"tmTheme"
                 (λ () (import-theme-file (fixture "nope.css"))))))

   (test-suite
    "entry plan"
    (test-case "dark scheme plan"
      (check-equal?
       (scheme-entry-plan
        '("P" #t
          ((keyword "#ff79c6" #t #f #f))
          ((background "#282a36") (foreground "#f8f8f2")
           (selection "#44475a") (paren-match "#44475a"))))
       '((framework:syntax-color:scheme:keyword dark ("#ff79c6" #t #f #f))
         (framework:basic-canvas-background dark "#282a36")
         (framework:default-text-color dark "#f8f8f2")
         (camp:vim-selection-color dark "#44475a")
         (framework:paren-match-color dark "#44475a"))))
    (test-case "light scheme plan omits absent globals"
      (check-equal?
       (scheme-entry-plan
        '("Q" #f
          ((comment "#998877" #f #t #f))
          ((background "#ffffff") (foreground "#222222"))))
       '((framework:syntax-color:scheme:comment light ("#998877" #f #t #f))
         (framework:basic-canvas-background light "#ffffff")
         (framework:default-text-color light "#222222"))))
    (test-case "globals without an entry are skipped"
      (check-equal?
       (scheme-entry-plan
        '("R" #t () ((background "#191724") (caret "#6e6a86"))))
       '((framework:basic-canvas-background dark "#191724"))))
    (test-case "entry universe covers the 19 token and 4 global entries"
      (check-equal? (length scheme-entry-names) 23)
      (for ([e (in-list '(framework:syntax-color:scheme:symbol
                          framework:syntax-color:scheme:hash-colon-keyword
                          framework:syntax-color:scheme:markup-heading
                          framework:basic-canvas-background
                          framework:paren-match-color
                          camp:vim-selection-color))])
        (check-not-false (memq e scheme-entry-names) (format "~a" e)))))))

(module+ main
  (run-tests theme-tests))

(module+ test
  (run-tests theme-tests))
