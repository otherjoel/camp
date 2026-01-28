#lang racket/base

;; CLI output formatting utilities

(require racket/format)

(provide format-duration
         with-timing
         right-align
         count-files-in-directory)

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
