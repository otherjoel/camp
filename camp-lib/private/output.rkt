#lang racket/base

;; CLI output formatting with color support

(require racket/file
         racket/format
         racket/match
         racket/path
         racket/string)

(provide format-duration
         use-color?
         color
         with-timing
         right-align
         count-files-in-directory
         ;; Convenience color functions
         dim
         green
         cyan
         yellow
         red
         bold)

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

;; ---------------------------------------------------------------------------
;; Timing

(define-syntax-rule (with-timing body ...)
  (let ([start (current-inexact-monotonic-milliseconds)])
    (define result (let () body ...))
    (define elapsed (- (current-inexact-monotonic-milliseconds) start))
    (values result elapsed)))

;; ---------------------------------------------------------------------------
;; Formatting

(define (format-duration ms)
  (define rounded (inexact->exact (round ms)))
  (if (< rounded 1000)
      (~a rounded "ms")
      (~a (~r (/ ms 1000.0) #:precision '(= 2) #:notation 'positional) "s")))

(define (right-align str width)
  (~a str #:min-width width #:align 'right))

;; ---------------------------------------------------------------------------
;; File utilities

(define (count-files-in-directory dir)
  (if (not (directory-exists? dir))
      0
      (for/sum ([item (in-directory dir)]
                #:when (file-exists? item))
        1)))
