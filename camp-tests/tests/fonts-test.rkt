#lang racket/base

;; Pure font-slot helpers (design §5). The GUI side (style application,
;; observables) is exercised by the app itself; these stay headless.

(require rackunit
         rackunit/text-ui
         camp/app/private/editor-utils)

(define fonts-tests
  (test-suite
   "font slot helpers"

   (test-case "cycling wraps 0 → 1 → 2 → 0"
     (check-equal? (next-font-slot 0) 1)
     (check-equal? (next-font-slot 1) 2)
     (check-equal? (next-font-slot 2) 0))

   (test-case "gutter size is 70% of the editor size, floored at 7"
     (check-equal? (gutter-font-size 13) 9)
     (check-equal? (gutter-font-size 12) 8)
     (check-equal? (gutter-font-size 10) 7)
     (check-equal? (gutter-font-size 8) 7)
     (check-equal? (gutter-font-size 20) 14))

   (test-case "slot labels"
     (check-equal? (font-slot-label 0 '("Menlo" 13)) "Slot 1: Menlo 13")
     (check-equal? (font-slot-label 2 '(#f #f)) "Slot 3: (default)")
     (check-equal? (font-slot-label 1 '("Triplicate T4c" #f)) "Slot 2: Triplicate T4c")
     (check-equal? (font-slot-label 1 '(#f 14)) "Slot 2: (default) 14"))))

(module+ main
  (run-tests fonts-tests))

(module+ test
  (run-tests fonts-tests))
