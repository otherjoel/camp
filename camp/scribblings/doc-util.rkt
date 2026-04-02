#lang debug racket/base

(require (for-syntax racket/base)
         racket/file
         racket/list
         racket/logging
         racket/match
         racket/runtime-path
         racket/string
         (except-in camp/private/log bold)
         raco/all-tools
         scribble/core
         scribble/decode
         scribble/example
         scribble/html-properties
         scribble/manual)

(provide (all-defined-out))

(define-runtime-path aux-css "scrbl-aux.css")

;;================================================
;; Sandbox

(define-runtime-path scribblings-dir "/")
(define-runtime-path sample-proj-dir "mysite/")

(define (sample-file filename)
  (build-path sample-proj-dir filename))

(define sandbox
  (parameterize ([current-directory sample-proj-dir])
    (make-base-eval #:lang 'racket/base)))

(define (clear-sandbox-compile-cache!)
  (delete-directory/files (sample-file "compiled") #:must-exist? #f))

;; (or/c 'gone 'exists) -> (or/c void? #f)
(define (ensure-sandbox-state state)
  (case state
    [(gone) (delete-directory/files sample-proj-dir #:must-exist? #f)]
    [(exists) (with-handlers ([exn:fail:filesystem? (λ (_exn) #f)]) (make-directory sample-proj-dir))]))

;; Call a raco command with args as if it had been run from the sample-proj directory.
;; For `raco camp` commands, returns a string containing the command output; for all
;; other commands, returns ""
(define (sandbox-raco command+args [working-dir sample-proj-dir])
  (match-define (cons command (app list->vector args)) (string-split command+args))
  (define cmd-mod (cadr (hash-ref (all-tools) command)))
  (with-logging-to-scribble
      (λ ()
        (parameterize ([current-command-line-arguments args]
                       [current-error-port (open-output-string)] ; keep output off the console
                       [current-namespace (make-base-namespace)]
                       [current-directory working-dir])
          (dynamic-require cmd-mod #f)))))

;;================================================
;; Logging to Scribble

;; ANSI code to CSS class mapping
(define ansi-code->class
  (hash "\033[0m"  #f           ; reset
        "\033[1m"  "term-bold"
        "\033[2m"  "term-dim"
        "\033[31m" "term-red"
        "\033[32m" "term-green"
        "\033[33m" "term-yellow"
        "\033[34m" "term-blue"
        "\033[35m" "term-magenta"
        "\033[36m" "term-cyan"
        "\033[37m" "term-white"
        "\033[92m" "term-bright-green"
        "\033[96m" "term-bright-cyan"))

;; Parse a string with ANSI codes into a list of Scribble content elements
(define (ansi-string->content str)
  (define ansi-pattern #rx"\033\\[[0-9;]+m")
  (let loop ([s str] [current-class #f] [acc '()])
    (define m (regexp-match-positions ansi-pattern s))
    (cond
      [(not m)
       ;; No more ANSI codes; wrap remaining text and return
       (reverse (if (string=? s "")
                    acc
                    (cons (wrap-with-class s current-class) acc)))]
      [else
       (match-define (cons (cons start end) _) m)
       (define before (substring s 0 start))
       (define code (substring s start end))
       (define after (substring s end))
       (define new-class (hash-ref ansi-code->class code current-class))
       (define new-acc
         (if (string=? before "")
             acc
             (cons (wrap-with-class before current-class) acc)))
       (loop after new-class new-acc)])))

;; Wrap text in a styled element if class is non-#f
(define (wrap-with-class text class)
   (if class
       (element (style class (list (css-style-addition aux-css))) (exec text))
       text))

;; Run thunk, capturing camp log messages as Scribble content.
;; Returns a list of content elements (one per log line) suitable for use with verbatim.
(define (with-logging-to-scribble thunk)
  (define log-lines '())
  (parameterize ([use-color? #t])
    (with-intercepted-logging
        (λ (vec)
          (define raw-msg (vector-ref vec 1))
          ;; Only capture messages that actually originate from camp logging
          (when (string-prefix? raw-msg "camp: ")
            (set! log-lines (cons (substring raw-msg 6) log-lines))))
      thunk
      'info 'camp))
  (add-between
   (for/list ([line (in-list (reverse log-lines))])
     (ansi-string->content line))
   "\n"))

;;================================================
;; Content

(define X-expression (tech #:doc '(lib "xml/xml.scrbl") "X-expression"))

(define (inline-note #:type [type 'note] . elems)
  (compound-paragraph
   (style "inline-note" (list (css-style-addition aux-css)
                              (attributes `((class . ,(format "refcontent ~a" type))))
                              (alt-tag "aside")))
   (decode-flow elems)))

;; Mark text as worthy of review for possible Congame improvements
(define (mark . elems)
  (element (style "review" (list (css-style-addition aux-css)
                                 (alt-tag "mark")))
           elems))

;; Mark filler content as placeholders for the real thing to be added later
(define (tktk . elems)
  (compound-paragraph
   (style "tktk" (list (css-style-addition aux-css) (alt-tag "div")))
   (decode-flow (cons (icon "Content to be added later" "⏳") elems))))

(define (icon tooltip str)
  (element (style "margin-icon" (list (attributes `([title ,@tooltip]))
                                      (alt-tag "abbr")))
           (list str)))

;; Style for sample terminal output
(define (terminal . args)
  (compound-paragraph (style "terminal" (list (color-property (list #x66 #x33 #x99))
                                              (css-style-addition aux-css)
                                              (alt-tag "div")))
                      (list (apply verbatim (flatten args)))))

;; Simulate a command-line prompt. Any prompt symbols/separators are handled in CSS.
(define (:> . elems)
  (element (style "prompt" (list (color-property (list 0 0 0))))
           (apply exec elems)))

;; Simulate a bash-style comment
(define (rem . args)
  (apply racketcommentfont (cons "# " args)))

(define (html-tag tag-name-str)
  (racketvalfont (format "<~a>" tag-name-str)))

;; Style text as a keyboard key or a button
(define (kbd . elems)
  (element (style "kbd" (list (css-style-addition aux-css)
                              (alt-tag "kbd")))
           elems))

;; Style for output in the DrRacket interactions window
(define (dr-message . elems)
  (element (style "dr-message" (list (css-style-addition aux-css)
                                     (alt-tag "span")))
           elems))

;; Simulate a browser window
(define (browser . elems)
  (compound-paragraph
   (style "browser" (list (css-style-addition aux-css)
                          (alt-tag "div")))
   (decode-flow elems)))

;; For use inside `browser`
(define (mock-textbox)
  (element (style "mock-textbox" (list (css-style-addition aux-css)
                                       (alt-tag "span")))
           " "))

;; Insert a screenshot, using a runtime path, centered and scaled down
(define-syntax (screenshot stx)
  (syntax-case stx ()
    [(_ name-path-str xs ...)
     (with-syntax ([name-id (datum->syntax stx (string->symbol (syntax-e #'name-path-str)))])
       #'(begin
           (define-runtime-path name-id (quote name-path-str))
           (centered
            (image-element (style "figure" (list (css-style-addition aux-css)))
                           '() name-id '() 0.4))))]))

(define-syntax (browser-screenshot stx)
  (syntax-case stx ()
    [(_ name-path-str xs ...)
     (with-syntax ([name-id (datum->syntax stx (string->symbol (syntax-e #'name-path-str)))])
       #'(begin
           (define-runtime-path name-id (quote name-path-str))
           (paragraph
            (style "browser" (list (css-style-addition aux-css)
                                   (alt-tag "div")))
            (image-element plain '() name-id '() 0.4))))]))

(define (youtube-embed-element src)
  (element
   (make-style
    "youtube-embed"
    (list
     (make-alt-tag "iframe")
     (make-attributes `((width           . "700")
                        (height          . "394")
                        (src             . ,src)
                        (frameborder     . "0")
                        (allowfullscreen . "")))))
   ""))
