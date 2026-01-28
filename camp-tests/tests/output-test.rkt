#lang racket/base

;; Tests for CLI output formatting

(require rackunit
         rackunit/text-ui
         camp/private/output
         (only-in camp/private/log use-color? color))

(define output-tests
  (test-suite
   "Output formatting"

   (test-suite
    "format-duration"

    (test-case "formats sub-second as milliseconds"
      (check-equal? (format-duration 0) "0ms")
      (check-equal? (format-duration 1) "1ms")
      (check-equal? (format-duration 15) "15ms")
      (check-equal? (format-duration 999) "999ms"))

    (test-case "formats >= 1 second with decimal"
      (check-equal? (format-duration 1000) "1.00s")
      (check-equal? (format-duration 1500) "1.50s")
      (check-equal? (format-duration 2340) "2.34s")
      (check-equal? (format-duration 10000) "10.00s"))

    (test-case "rounds fractional milliseconds"
      (check-equal? (format-duration 15.4) "15ms")
      (check-equal? (format-duration 15.6) "16ms")
      (check-equal? (format-duration 1234.5) "1.23s")))

   (test-suite
    "ANSI color helpers"

    (test-case "color wraps text with ANSI codes when enabled"
      (parameterize ([use-color? #t])
        (check-regexp-match #rx"\033\\[" (color 'green "text"))
        (check-regexp-match #rx"text" (color 'green "text"))
        (check-regexp-match #rx"\033\\[0m$" (color 'green "text"))))

    (test-case "color returns plain text when disabled"
      (parameterize ([use-color? #f])
        (check-equal? (color 'green "text") "text")
        (check-equal? (color 'cyan "hello") "hello"))))

   (test-suite
    "with-timing macro"

    (test-case "returns timing and result"
      (define-values (result ms) (with-timing (+ 1 2)))
      (check-equal? result 3)
      (check-pred number? ms)
      (check-true (>= ms 0)))

    (test-case "captures actual elapsed time"
      (define-values (result ms) (with-timing (sleep 0.05) 'done))
      (check-equal? result 'done)
      (check-true (>= ms 40))))  ; at least 40ms for 50ms sleep

   (test-suite
    "right-align"

    (test-case "pads string to width"
      (check-equal? (right-align "15ms" 10) "      15ms")
      (check-equal? (right-align "1.50s" 10) "     1.50s"))

    (test-case "returns string unchanged if wider than width"
      (check-equal? (right-align "12345" 3) "12345")))))

(module+ main
  (run-tests output-tests))

(module+ test
  (require rackunit/text-ui)
  (run-tests output-tests))
