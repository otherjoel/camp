#lang racket/base

;; Tests for camp/private/draft module (draft post creation)

(require rackunit
         racket/file
         racket/path
         racket/string
         camp
         (only-in camp/private/draft
                  create-draft
                  generate-draft-content
                  title->filename))

;; ---------------------------------------------------------------------------
;; Test fixture path

(define fixture-site-path
  (simplify-path
   (build-path (path-only (syntax-source #'here))
               "fixtures" "test-site" "site.rkt")))

(define fixture-site (load-site fixture-site-path))

;; ---------------------------------------------------------------------------
;; title->filename tests

(check-equal? (title->filename "Hello World" ".md.rkt")
              "hello-world.md.rkt")

(check-equal? (title->filename "My First Post!" ".md.rkt")
              "my-first-post.md.rkt")

(check-equal? (title->filename "  Spaces  Everywhere  " ".md.rkt")
              "spaces-everywhere.md.rkt")

(check-equal? (title->filename "Special: Characters & Symbols?" ".md.rkt")
              "special-characters-symbols.md.rkt")

(check-equal? (title->filename "2024 Year in Review" ".md.rkt")
              "2024-year-in-review.md.rkt")

(check-equal? (title->filename "Already-Slugified" ".page.rkt")
              "already-slugified.page.rkt")

;; ---------------------------------------------------------------------------
;; generate-draft-content tests

(define test-content (generate-draft-content "My Test Post" "2024-06-15" "my-test-package"))

(check-true (string-contains? test-content "#lang punct my-test-package"))
(check-true (string-contains? test-content "title: My Test Post"))
(check-true (string-contains? test-content "date: 2024-06-15"))
(check-true (string-contains? test-content "draft?: true"))

;; Check YAML front matter structure
(check-true (string-prefix? test-content "#lang punct"))
(check-true (string-contains? test-content "\n---\n"))

;; Content without package name
(define test-content-no-pkg (generate-draft-content "Post" "2024-01-01" #f))
(check-true (string-prefix? test-content-no-pkg "#lang punct\n"))
(check-false (string-contains? test-content-no-pkg "#lang punct "))

;; ---------------------------------------------------------------------------
;; create-draft tests (with temp directory)

(define temp-dir (make-temporary-file "camp-draft-test-~a" 'directory))

(define (cleanup!)
  (when (directory-exists? temp-dir)
    (delete-directory/files temp-dir)))

(define (with-temp-site thunk)
  (dynamic-wind
    (λ ()
      (make-directory* (build-path temp-dir "blog"))
      (make-directory* (build-path temp-dir "pages")))
    thunk
    cleanup!))

;; Test creating a draft in the first collection
(with-temp-site
  (λ ()
    (define test-site
      (hash-set* fixture-site
                 'root temp-dir
                 'racket-collection "test-site-pkg"))

    (define result (create-draft test-site "My Draft Title"))

    ;; Should return a path
    (check-pred path? result)

    ;; File should exist
    (check-true (file-exists? result))

    ;; Should be in blog directory (first collection)
    (define result-parts (explode-path result))
    (check-not-false (member (string->path "blog") result-parts)
                     "Path should contain 'blog' directory")

    ;; Filename should be slugified
    (check-equal? (path->string (file-name-from-path result))
                  "my-draft-title.md.rkt")

    ;; Content should be correct
    (define content (file->string result))
    (check-true (string-contains? content "#lang punct test-site-pkg"))
    (check-true (string-contains? content "title: My Draft Title"))
    (check-true (string-contains? content "draft?: true"))))

;; Test creating a draft in a specific collection
(with-temp-site
  (λ ()
    (define test-site
      (hash-set* fixture-site
                 'root temp-dir
                 'racket-collection "test-site-pkg"))

    (define result (create-draft test-site "About Page" #:collection "pages"))

    ;; Should be in pages directory
    (define result-parts (explode-path result))
    (check-not-false (member (string->path "pages") result-parts)
                     "Path should contain 'pages' directory")))

;; Test error when collection not found
(with-temp-site
  (λ ()
    (define test-site
      (hash-set* fixture-site
                 'root temp-dir
                 'racket-collection "test-site-pkg"))

    (check-exn
     exn:fail?
     (λ () (create-draft test-site "Test" #:collection "nonexistent")))))

;; Test that duplicate filenames get numeric suffix
(with-temp-site
  (λ ()
    (define test-site
      (hash-set* fixture-site
                 'root temp-dir
                 'racket-collection "test-site-pkg"))

    ;; Create first draft
    (define first (create-draft test-site "Duplicate Title"))
    (check-equal? (path->string (file-name-from-path first))
                  "duplicate-title.md.rkt")

    ;; Create second draft with same title
    (define second (create-draft test-site "Duplicate Title"))
    (check-equal? (path->string (file-name-from-path second))
                  "duplicate-title-2.md.rkt")

    ;; Create third draft with same title
    (define third (create-draft test-site "Duplicate Title"))
    (check-equal? (path->string (file-name-from-path third))
                  "duplicate-title-3.md.rkt")))
