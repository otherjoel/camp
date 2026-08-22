#lang racket/base

;; Tests for the in-app editor's completion engine (headless)

(require racket/list
         rackunit
         rackunit/text-ui
         camp/app/private/complete)

(define (wait-for-completions path #:timeout [secs 60])
  (let loop ([tries (* secs 10)])
    (define words (completions-for path))
    (cond
      [(pair? words) words]
      [(zero? tries) '()]
      [else (sleep 0.1) (loop (sub1 tries))])))

(define (temp-source name)
  (build-path (find-system-path 'temp-dir) name))

(define editor-complete-tests
  (test-suite
   "Editor completion engine"

   (test-case "expansion yields local definitions and required exports"
     (define p (temp-source "camp-complete-a.rkt"))
     (request-completions!
      p "#lang racket/base\n(require racket/string)\n(define my-special-marker 42)\n")
     (define words (wait-for-completions p))
     (check-not-false (member "my-special-marker" words))
     (check-not-false (member "string-join" words))
     (check-not-false (member "define" words)))

   (test-case "failed expansion keeps the previous completions"
     (define p (temp-source "camp-complete-a.rkt"))
     (define q (temp-source "camp-complete-b.rkt"))
     (request-completions! p "#lang racket/base\n(define (broken")
     (request-completions! q "#lang racket/base\n(define sentinel-after-bad 1)\n")
     (check-not-false (member "sentinel-after-bad" (wait-for-completions q)))
     (check-not-false (member "my-special-marker" (completions-for p))))

   (test-case "punct documents expand"
     (define p (temp-source "camp-complete-doc.md.rkt"))
     (request-completions!
      p "#lang punct\n---\ntitle: Test\n---\n\nHello •(number->string 42) world\n")
     (check-pred pair? (wait-for-completions p)))

   (test-case "buffer words harvests identifier-shaped tokens"
     (define words (buffer-words "output-folder = \"public\"\n•(define-meta cool-title \"Yes\")\n"))
     (check-not-false (member "output-folder" words))
     (check-not-false (member "define-meta" words))
     (check-not-false (member "cool-title" words))
     (check-false (member "=" words))
     (check-equal? words (remove-duplicates words)))))

(module+ main
  (run-tests editor-complete-tests))

(module+ test
  (run-tests editor-complete-tests))
