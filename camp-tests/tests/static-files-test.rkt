#lang racket/base

;; Tests for static file copying functionality

(require rackunit
         racket/file
         racket/path
         camp/private/build)

;; ---------------------------------------------------------------------------
;; Test fixture paths

(define fixture-site-root
  (simplify-path
   (build-path (path-only (syntax-source #'here))
               "fixtures" "test-site")))

(define fixture-static-dir (build-path fixture-site-root "static"))

;; Use a temp directory for test output
(define (make-test-output-dir)
  (make-temporary-file "camp-test-output~a" 'directory))

;; ---------------------------------------------------------------------------
;; copy-static-files tests

(define test-output-dir (make-test-output-dir))

;; Copy static files to temp directory
(copy-static-files fixture-static-dir test-output-dir)

;; Check that root-level file was copied
(check-true (file-exists? (build-path test-output-dir "style.css"))
            "Root static file should be copied")

;; Check that nested directory structure is preserved
(check-true (directory-exists? (build-path test-output-dir "css"))
            "Nested directories should be created")

(check-true (file-exists? (build-path test-output-dir "css" "extra.css"))
            "Nested static files should be copied")

;; Check file contents are preserved
(check-equal? (file->string (build-path test-output-dir "style.css"))
              (file->string (build-path fixture-static-dir "style.css"))
              "File contents should be preserved")

(check-equal? (file->string (build-path test-output-dir "css" "extra.css"))
              (file->string (build-path fixture-static-dir "css" "extra.css"))
              "Nested file contents should be preserved")

;; ---------------------------------------------------------------------------
;; Test copying to existing directory (should overwrite)

(define test-output-dir-2 (make-test-output-dir))

;; Pre-create a file that will be overwritten
(make-directory* (build-path test-output-dir-2))
(call-with-output-file (build-path test-output-dir-2 "style.css")
  (lambda (out) (display "old content" out))
  #:exists 'replace)

;; Copy should overwrite
(copy-static-files fixture-static-dir test-output-dir-2)

(check-equal? (file->string (build-path test-output-dir-2 "style.css"))
              (file->string (build-path fixture-static-dir "style.css"))
              "Existing files should be overwritten")

;; ---------------------------------------------------------------------------
;; Test with non-existent static folder (should not error)

(define test-output-dir-3 (make-test-output-dir))
(define non-existent-static (build-path fixture-site-root "no-such-static"))

;; Should not raise an error when static folder doesn't exist
(check-not-exn
 (lambda () (copy-static-files non-existent-static test-output-dir-3))
 "Should handle non-existent static folder gracefully")

;; ---------------------------------------------------------------------------
;; Cleanup temp directories

(delete-directory/files test-output-dir #:must-exist? #f)
(delete-directory/files test-output-dir-2 #:must-exist? #f)
(delete-directory/files test-output-dir-3 #:must-exist? #f)
