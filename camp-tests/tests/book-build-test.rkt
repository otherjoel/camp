#lang racket/base

;; Tests for simplified book build pipeline

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         punct/doc
         camp/private/book-build
         camp/private/structs)

(define book-build-tests
  (test-suite
   "Book build tests"

   ;; -------------------------------------------------------------------------
   (test-suite
    "chapter and part structures"

    (test-case "chapter has slug and doc"
      (define doc (document (hasheq 'title "Test") '() '()))
      (define ch (hasheq 'slug "test-chapter" 'doc doc))
      (check-equal? (chapter-slug ch) "test-chapter")
      (check-equal? (chapter-doc ch) doc))

    (test-case "part has name and chapters"
      (define doc (document (hasheq 'title "Test") '() '()))
      (define ch (hasheq 'slug "ch1" 'doc doc))
      (define p (hasheq 'name "Part One" 'chapters (list ch)))
      (check-equal? (part-name p) "Part One")
      (check-equal? (length (part-chapters p)) 1)
      (check-equal? (chapter-slug (car (part-chapters p))) "ch1")))

   ;; -------------------------------------------------------------------------
   (test-suite
    "gather-book-parts"

    ;; Integration tests for gather-book-parts would require a full site setup.
    ;; These are tested via the demo site build.
    )

   ;; -------------------------------------------------------------------------
   (test-suite
    "copy-includes!"

    (test-case "copies single file"
      (define tmp-root (make-temporary-file "camp-test-~a" 'directory))
      (define tmp-output (build-path tmp-root "output"))
      (make-directory* tmp-output)
      ;; Create a source file
      (define src-file (build-path tmp-root "template.typ"))
      (call-with-output-file src-file
        (λ (out) (display "#let book = 1" out)))
      ;; Copy it
      (copy-includes! (list "template.typ") tmp-root tmp-output)
      ;; Verify
      (check-true (file-exists? (build-path tmp-output "template.typ")))
      (check-equal? (file->string (build-path tmp-output "template.typ"))
                    "#let book = 1")
      ;; Cleanup
      (delete-directory/files tmp-root))

    (test-case "copies directory recursively"
      (define tmp-root (make-temporary-file "camp-test-~a" 'directory))
      (define tmp-output (build-path tmp-root "output"))
      (make-directory* tmp-output)
      ;; Create a directory with files
      (define fonts-dir (build-path tmp-root "fonts"))
      (make-directory* fonts-dir)
      (call-with-output-file (build-path fonts-dir "main.ttf")
        (λ (out) (display "font-data" out)))
      ;; Copy it
      (copy-includes! (list "fonts") tmp-root tmp-output)
      ;; Verify
      (check-true (directory-exists? (build-path tmp-output "fonts")))
      (check-true (file-exists? (build-path tmp-output "fonts" "main.ttf")))
      ;; Cleanup
      (delete-directory/files tmp-root))

    (test-case "handles empty includes list"
      (define tmp-root (make-temporary-file "camp-test-~a" 'directory))
      (define tmp-output (build-path tmp-root "output"))
      (make-directory* tmp-output)
      ;; Should not error
      (copy-includes! '() tmp-root tmp-output)
      ;; Cleanup
      (delete-directory/files tmp-root)))))

(module+ main
  (run-tests book-build-tests))

(module+ test
  (require rackunit/text-ui)
  (run-tests book-build-tests))
