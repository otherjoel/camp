#lang racket/base

;; Tests for ANSI escape sequence parsing

(require rackunit
         rackunit/text-ui
         camp/private/ansi)

(define ansi-tests
  (test-suite
   "ANSI parsing"

   (test-suite
    "parse-ansi"

    (test-case "plain text returns single unstyled span"
      (check-equal? (parse-ansi "hello world")
                    '(("hello world" . #f))))

    (test-case "empty string returns empty list"
      (check-equal? (parse-ansi "") '()))

    (test-case "single color code"
      (check-equal? (parse-ansi "\033[32mhello\033[0m")
                    '(("hello" . green))))

    (test-case "styled text with surrounding plain text"
      (check-equal? (parse-ansi "before \033[31mred\033[0m after")
                    '(("before " . #f) ("red" . red) (" after" . #f))))

    (test-case "sequential colors without explicit reset"
      (check-equal? (parse-ansi "\033[32mgreen\033[31mred\033[0m")
                    '(("green" . green) ("red" . red))))

    (test-case "dim and bold attributes"
      (check-equal? (parse-ansi "\033[2mdimmed\033[0m \033[1mbold\033[0m")
                    '(("dimmed" . dim) (" " . #f) ("bold" . bold))))

    (test-case "empty text between codes produces no span"
      (check-equal? (parse-ansi "\033[0m\033[32mhello\033[0m")
                    '(("hello" . green))))

    (test-case "bright color codes"
      (check-equal? (parse-ansi "\033[92mhi\033[0m")
                    '(("hi" . bright-green)))
      (check-equal? (parse-ansi "\033[96mhi\033[0m")
                    '(("hi" . bright-cyan))))

    (test-case "unknown codes are silently ignored"
      (check-equal? (parse-ansi "\033[99mhello\033[0m")
                    '(("hello" . #f))))

    (test-case "text ending without reset keeps style in final span"
      (check-equal? (parse-ansi "ok \033[33myellow")
                    '(("ok " . #f) ("yellow" . yellow))))

    (test-case "only escape codes, no visible text"
      (check-equal? (parse-ansi "\033[32m\033[0m") '())))))

(module+ main
  (run-tests ansi-tests))

(module+ test
  (require rackunit/text-ui)
  (run-tests ansi-tests))
