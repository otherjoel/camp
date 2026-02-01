#lang racket/base

;; Tests for camp/private/collections module (source discovery)

(require rackunit
         racket/path
         racket/list
         punct/doc
         (only-in camp/private/collections
                  source-pattern->directory
                  is-source?
                  find-sources
                  in-sources
                  path->slug))

;; ---------------------------------------------------------------------------
;; Test fixture path

(define fixture-site-root
  (simplify-path
   (build-path (path-only (syntax-source #'here))
               "fixtures" "test-site")))

(define fixture-blog-dir (build-path fixture-site-root "blog"))
(define fixture-pages-dir (build-path fixture-site-root "pages"))

;; ---------------------------------------------------------------------------
;; source-pattern->directory tests

(check-equal? (path->string (source-pattern->directory "blog/*")) "blog")
(check-equal? (path->string (source-pattern->directory "pages/*")) "pages")
(check-equal? (path->string (source-pattern->directory "deep/nested/path/*"))
              (path->string (build-path "deep" "nested" "path")))

;; Pattern "*" means current directory
(check-pred path? (source-pattern->directory "*"))

;; ---------------------------------------------------------------------------
;; is-source? tests

(check-true (is-source? "foo.md.rkt" ".md.rkt"))
(check-true (is-source? (string->path "foo.md.rkt") ".md.rkt"))
(check-true (is-source? "blog/foo.md.rkt" ".md.rkt"))

;; Wrong extension
(check-false (is-source? "foo.txt" ".md.rkt"))
(check-false (is-source? "foo.rkt" ".md.rkt"))

;; .page.rkt is always recognized regardless of configured extension
(check-true (is-source? "foo.page.rkt" ".md.rkt"))
(check-true (is-source? "bar.page.rkt" ".txt.rkt"))
(check-true (is-source? "pages/about.page.rkt" ".md.rkt"))

;; Emacs backup files should be excluded
(check-false (is-source? ".#foo.md.rkt" ".md.rkt"))
(check-false (is-source? "blog/.#foo.md.rkt" ".md.rkt"))
(check-false (is-source? ".#foo.page.rkt" ".md.rkt"))

;; ---------------------------------------------------------------------------
;; path->slug tests

(check-equal? (path->slug "my-post.md.rkt" ".md.rkt") "my-post")
(check-equal? (path->slug (string->path "my-post.md.rkt") ".md.rkt") "my-post")
(check-equal? (path->slug "blog/my-post.md.rkt" ".md.rkt") "my-post")
(check-equal? (path->slug "complex-name-here.md.rkt" ".md.rkt") "complex-name-here")

;; Different extension
(check-equal? (path->slug "page.page.rkt" ".page.rkt") "page")

;; ---------------------------------------------------------------------------
;; find-sources tests

(define blog-sources (find-sources fixture-blog-dir ".md.rkt"))

(check-equal? (length blog-sources) 4 "Should find 4 blog posts (including draft)")

;; All paths should be absolute
(check-true (andmap absolute-path? blog-sources))

;; All paths should end with .md.rkt
(check-true (andmap (lambda (p) (path-has-extension? p #".md.rkt")) blog-sources))

;; Verify expected files are found
(define blog-filenames (map (compose path->string file-name-from-path) blog-sources))
(check-not-false (member "first-post.md.rkt" blog-filenames))
(check-not-false (member "second-post.md.rkt" blog-filenames))
(check-not-false (member "third-post.md.rkt" blog-filenames))

;; Pages collection
(define pages-sources (find-sources fixture-pages-dir ".md.rkt"))
(check-equal? (length pages-sources) 2 "Should find 2 pages (about, home)")

;; ---------------------------------------------------------------------------
;; in-sources tests

(define collected-posts
  (for/list ([(path slug doc) (in-sources fixture-blog-dir ".md.rkt")])
    (list slug (hash-ref (document-metas doc) 'title))))

(check-equal? (length collected-posts) 4)

;; Check that slugs are correctly extracted
(define slugs (map car collected-posts))
(check-not-false (member "first-post" slugs))
(check-not-false (member "second-post" slugs))
;; third-post has custom slug in metadata
(check-not-false (member "custom-slug" slugs))
(check-false (member "third-post" slugs))

;; Check that titles are loaded
(define titles (map cadr collected-posts))
(check-not-false (member "First Post" titles))
(check-not-false (member "Second Post" titles))
(check-not-false (member "Third Post" titles))

;; Check that metadata is correctly loaded
(for ([(path slug doc) (in-sources fixture-blog-dir ".md.rkt")])
  (define metas (document-metas doc))
  (when (equal? slug "second-post")
    (check-equal? (hash-ref metas 'tags) "beta, gamma")
    (check-equal? (hash-ref metas 'series) "tutorials")
    ;; Punct YAML parses ISO dates as strings
    (check-equal? (hash-ref metas 'date) "2024-02-15")))
