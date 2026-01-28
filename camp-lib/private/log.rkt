#lang racket/base

(require racket/logging
         racket/string)

(provide camp-logger
         log-camp-fatal
         log-camp-error
         log-camp-warning
         log-camp-info
         log-camp-debug
         with-logging-to-stderr
         ;; Color support
         use-color?
         color
         dim
         green
         cyan
         yellow
         red
         bold)

(define-logger camp)

;; Run thunk, printing camp log messages to stderr (without "camp: " prefix)
(define (with-logging-to-stderr thunk)
  (with-intercepted-logging
    (λ (vec)
      (define raw-msg (vector-ref vec 1))
      (when (string-prefix? raw-msg "camp: ")
        (displayln (substring raw-msg 6) (current-error-port))))
    thunk
    #:logger camp-logger
    'info 'camp))

;; ---------------------------------------------------------------------------
;; Color support

(define use-color?
  (make-parameter (terminal-port? (current-output-port))))

(define ansi-codes
  (hasheq 'reset   "\033[0m"
          'bold    "\033[1m"
          'dim     "\033[2m"
          'red     "\033[31m"
          'green   "\033[32m"
          'yellow  "\033[33m"
          'blue    "\033[34m"
          'magenta "\033[35m"
          'cyan    "\033[36m"
          'white   "\033[37m"
          'bright-green  "\033[92m"
          'bright-cyan   "\033[96m"))

(define (color code text)
  (if (use-color?)
      (string-append (hash-ref ansi-codes code "")
                     text
                     (hash-ref ansi-codes 'reset))
      text))

(define (dim text)    (color 'dim text))
(define (green text)  (color 'green text))
(define (cyan text)   (color 'cyan text))
(define (yellow text) (color 'yellow text))
(define (red text)    (color 'red text))
(define (bold text)   (color 'bold text))
