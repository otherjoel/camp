#lang racket/base

;; Tests for page ordering functionality (task 2.3)
;; - Sorting pages by sort-key (default: date)
;; - Ascending/descending order
;; - prev-in/next-in navigation
;; - Missing sort-key error handling

(require rackunit
         racket/list
         racket/path
         punct/doc
         gregor
         camp/private/structs
         camp/private/collections
         camp/private/main)

;; ---------------------------------------------------------------------------
;; Test fixture path

(define fixture-site-root
  (simplify-path
   (build-path (path-only (syntax-source #'here))
               "fixtures" "test-site")))

(define fixture-blog-dir (build-path fixture-site-root "blog"))

;; ---------------------------------------------------------------------------
;; Helper: create a mock document with given metadata

(define (make-mock-doc metas)
  (document metas '() '()))

;; ---------------------------------------------------------------------------
;; parse-sort-value tests

(test-case "parse-sort-value: parses ISO date strings to gregor dates"
  (check-pred date? (parse-sort-value "2024-01-20" "date"))
  (check-equal? (date->iso8601 (parse-sort-value "2024-01-20" "date"))
                "2024-01-20"))

(test-case "parse-sort-value: returns non-date strings as-is for non-date keys"
  (check-equal? (parse-sort-value "some-title" "title") "some-title")
  (check-equal? (parse-sort-value "zebra" "name") "zebra"))

(test-case "parse-sort-value: handles gregor date objects (pass-through)"
  (define d (date 2024 3 15))
  (check-equal? (parse-sort-value d "date") d))

;; ---------------------------------------------------------------------------
;; sort-pages tests

(test-case "sort-pages: sorts by date descending (default)"
  (define pages
    (list (list #f "post-a" (make-mock-doc (hasheq 'date "2024-01-20" 'title "A")))
          (list #f "post-c" (make-mock-doc (hasheq 'date "2024-03-10" 'title "C")))
          (list #f "post-b" (make-mock-doc (hasheq 'date "2024-02-15" 'title "B")))))
  (define coll (hasheq 'name "blog"
                       'source "blog/*"
                       'output-paths "*/"
                       'order "descending"
                       'sort-key "date"
                       'taxonomies '()))
  (define sorted (sort-pages pages coll))
  (check-equal? (map cadr sorted) '("post-c" "post-b" "post-a")))

(test-case "sort-pages: sorts by date ascending"
  (define pages
    (list (list #f "post-a" (make-mock-doc (hasheq 'date "2024-01-20" 'title "A")))
          (list #f "post-c" (make-mock-doc (hasheq 'date "2024-03-10" 'title "C")))
          (list #f "post-b" (make-mock-doc (hasheq 'date "2024-02-15" 'title "B")))))
  (define coll (hasheq 'name "blog"
                       'source "blog/*"
                       'output-paths "*/"
                       'order "ascending"
                       'sort-key "date"
                       'taxonomies '()))
  (define sorted (sort-pages pages coll))
  (check-equal? (map cadr sorted) '("post-a" "post-b" "post-c")))

(test-case "sort-pages: sorts by title ascending"
  (define pages
    (list (list #f "post-z" (make-mock-doc (hasheq 'title "Zebra")))
          (list #f "post-a" (make-mock-doc (hasheq 'title "Apple")))
          (list #f "post-m" (make-mock-doc (hasheq 'title "Mango")))))
  (define coll (hasheq 'name "pages"
                       'source "pages/*"
                       'output-paths "*/"
                       'order "ascending"
                       'sort-key "title"
                       'taxonomies '()))
  (define sorted (sort-pages pages coll))
  (check-equal? (map cadr sorted) '("post-a" "post-m" "post-z")))

(test-case "sort-pages: sorts by title descending"
  (define pages
    (list (list #f "post-z" (make-mock-doc (hasheq 'title "Zebra")))
          (list #f "post-a" (make-mock-doc (hasheq 'title "Apple")))
          (list #f "post-m" (make-mock-doc (hasheq 'title "Mango")))))
  (define coll (hasheq 'name "pages"
                       'source "pages/*"
                       'output-paths "*/"
                       'order "descending"
                       'sort-key "title"
                       'taxonomies '()))
  (define sorted (sort-pages pages coll))
  (check-equal? (map cadr sorted) '("post-z" "post-m" "post-a")))

(test-case "sort-pages: raises error when sort-key is missing from a page"
  (define pages
    (list (list #f "post-a" (make-mock-doc (hasheq 'date "2024-01-20" 'title "A")))
          (list #f "post-b" (make-mock-doc (hasheq 'title "B")))  ; missing date!
          (list #f "post-c" (make-mock-doc (hasheq 'date "2024-03-10" 'title "C")))))
  (define coll (hasheq 'name "blog"
                       'source "blog/*"
                       'output-paths "*/"
                       'order "descending"
                       'sort-key "date"
                       'taxonomies '()))
  (check-exn
   (regexp "missing required metadata key \"date\"")
   (lambda () (sort-pages pages coll))))

(test-case "sort-pages: stable sort preserves order for equal values"
  ;; When two pages have the same sort value, their relative order should be preserved
  (define pages
    (list (list #f "first" (make-mock-doc (hasheq 'date "2024-01-20" 'title "First")))
          (list #f "second" (make-mock-doc (hasheq 'date "2024-01-20" 'title "Second")))
          (list #f "third" (make-mock-doc (hasheq 'date "2024-01-20" 'title "Third")))))
  (define coll (hasheq 'name "blog"
                       'source "blog/*"
                       'output-paths "*/"
                       'order "descending"
                       'sort-key "date"
                       'taxonomies '()))
  (define sorted (sort-pages pages coll))
  ;; With stable sort and descending, original order should be preserved
  (check-equal? (map cadr sorted) '("first" "second" "third")))

;; ---------------------------------------------------------------------------
;; prev-in / next-in tests (with slug-based comparison)

(test-case "prev-in: returns previous page by slug"
  (define pages
    (list (page-link "/blog/2024/03/c/" "C" (hasheq 'slug "c"))
          (page-link "/blog/2024/02/b/" "B" (hasheq 'slug "b"))
          (page-link "/blog/2024/01/a/" "A" (hasheq 'slug "a"))))
  (define prev (prev-in pages "b"))
  (check-equal? (page-link-title prev) "C"))

(test-case "prev-in: returns #f for first page"
  (define pages
    (list (page-link "/blog/2024/03/c/" "C" (hasheq 'slug "c"))
          (page-link "/blog/2024/02/b/" "B" (hasheq 'slug "b"))
          (page-link "/blog/2024/01/a/" "A" (hasheq 'slug "a"))))
  (check-false (prev-in pages "c")))

(test-case "prev-in: returns #f when slug not found"
  (define pages
    (list (page-link "/blog/2024/03/c/" "C" (hasheq 'slug "c"))
          (page-link "/blog/2024/02/b/" "B" (hasheq 'slug "b"))))
  (check-false (prev-in pages "nonexistent")))

(test-case "next-in: returns next page by slug"
  (define pages
    (list (page-link "/blog/2024/03/c/" "C" (hasheq 'slug "c"))
          (page-link "/blog/2024/02/b/" "B" (hasheq 'slug "b"))
          (page-link "/blog/2024/01/a/" "A" (hasheq 'slug "a"))))
  (define next (next-in pages "b"))
  (check-equal? (page-link-title next) "A"))

(test-case "next-in: returns #f for last page"
  (define pages
    (list (page-link "/blog/2024/03/c/" "C" (hasheq 'slug "c"))
          (page-link "/blog/2024/02/b/" "B" (hasheq 'slug "b"))
          (page-link "/blog/2024/01/a/" "A" (hasheq 'slug "a"))))
  (check-false (next-in pages "a")))

(test-case "next-in: returns #f when slug not found"
  (define pages
    (list (page-link "/blog/2024/03/c/" "C" (hasheq 'slug "c"))
          (page-link "/blog/2024/02/b/" "B" (hasheq 'slug "b"))))
  (check-false (next-in pages "nonexistent")))

;; ---------------------------------------------------------------------------
;; Integration test: sorting pages from actual fixture files

(test-case "sort-pages: works with actual fixture blog posts"
  ;; Load pages from fixtures using in-sources
  (define pages
    (for/list ([(path slug doc) (in-sources fixture-blog-dir ".md.rkt")])
      (list path slug doc)))
  (define coll (hasheq 'name "blog"
                       'source "blog/*"
                       'output-paths "blog/[yyyy]/[MM]/*/"
                       'order "descending"
                       'sort-key "date"
                       'taxonomies '("tags" "series")))
  (define sorted (sort-pages pages coll))
  (define slugs (map cadr sorted))
  ;; Dates are: first=2024-01-20, second=2024-02-15, third=2024-03-10
  ;; Descending order: third (custom-slug), second, first
  (check-equal? slugs '("custom-slug" "second-post" "first-post")))

(test-case "sort-pages: ascending order with fixture blog posts"
  (define pages
    (for/list ([(path slug doc) (in-sources fixture-blog-dir ".md.rkt")])
      (list path slug doc)))
  (define coll (hasheq 'name "blog"
                       'source "blog/*"
                       'output-paths "blog/[yyyy]/[MM]/*/"
                       'order "ascending"
                       'sort-key "date"
                       'taxonomies '("tags" "series")))
  (define sorted (sort-pages pages coll))
  (define slugs (map cadr sorted))
  ;; Ascending order: first, second, third (custom-slug)
  (check-equal? slugs '("first-post" "second-post" "custom-slug")))
