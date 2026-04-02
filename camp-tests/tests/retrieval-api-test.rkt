#lang racket/base

;; Tests for the document retrieval API (Phase 4)
;;
;; Tests get-collection, get-taxonomy-terms, get-taxonomy-pages
;; These functions require current-site-info to be parameterized.

(require rackunit
         racket/list
         racket/path
         punct/doc
         camp
         camp/build)

;; ---------------------------------------------------------------------------
;; Test fixture paths

(define fixture-site-root
  (simplify-path
   (build-path (path-only (syntax-source #'here))
               "fixtures" "test-site")))

(define fixture-site-path
  (build-path fixture-site-root "site.rkt"))

;; ---------------------------------------------------------------------------
;; Helper to set up test environment

(define (call-with-site-info thunk)
  (define site (load-site fixture-site-path))
  (define info (collect site))
  (parameterize ([current-site-info info])
    (thunk)))

;; ===========================================================================
;; get-collection tests
;; ===========================================================================

(test-case "get-collection: returns page-links for named collection"
  (call-with-site-info
   (λ ()
     (define blog-pages (get-collection "blog"))
     (check-pred list? blog-pages)
     (check-equal? (length blog-pages) 3)  ; excludes draft post by default
     (for ([pl (in-list blog-pages)])
       (check-pred page-link? pl)))))

(test-case "get-collection: pages are in collection sort order"
  (call-with-site-info
   (λ ()
     (define blog-pages (get-collection "blog"))
     ;; Blog is sorted descending by date; draft excluded by default
     (define titles (map page-link-title blog-pages))
     (check-equal? titles '("Third Post" "Second Post" "First Post")))))

(test-case "get-collection: respects #:limit"
  (call-with-site-info
   (λ ()
     (define blog-pages (get-collection "blog" #:limit 2))
     (check-equal? (length blog-pages) 2)
     (define titles (map page-link-title blog-pages))
     (check-equal? titles '("Third Post" "Second Post")))))

(test-case "get-collection: #:limit larger than collection returns all"
  (call-with-site-info
   (λ ()
     (define blog-pages (get-collection "blog" #:limit 100))
     (check-equal? (length blog-pages) 3))))

(test-case "get-collection: #:full-docs? returns documents"
  (call-with-site-info
   (λ ()
     (define docs (get-collection "blog" #:full-docs? #t))
     (check-pred list? docs)
     (check-equal? (length docs) 3)
     (for ([doc (in-list docs)])
       (check-pred document? doc)))))

(test-case "get-collection: page-link includes url, title, metas"
  (call-with-site-info
   (λ ()
     (define blog-pages (get-collection "blog"))
     (define first-page (last blog-pages)) ; first-post is last in descending order
     (check-equal? (page-link-title first-page) "First Post")
     (check-true (string? (page-link-url first-page)))
     (check-pred hash? (page-link-metas first-page))
     (check-equal? (hash-ref (page-link-metas first-page) 'slug) "first-post"))))

(test-case "get-collection: errors when not in build context"
  (check-exn
   exn:fail?
   (λ () (get-collection "blog"))))

(test-case "get-collection: returns empty list for unknown collection"
  (call-with-site-info
   (λ ()
     (check-equal? (get-collection "nonexistent") '()))))

(test-case "get-collection: #:include-drafts? includes draft pages"
  (call-with-site-info
   (λ ()
     (define blog-pages (get-collection "blog" #:include-drafts? #t))
     (check-equal? (length blog-pages) 4)
     (define titles (map page-link-title blog-pages))
     (check-not-false (member "Draft Post" titles)))))

(test-case "get-collection: #:include-drafts? with #:full-docs?"
  (call-with-site-info
   (λ ()
     (define docs (get-collection "blog" #:include-drafts? #t #:full-docs? #t))
     (check-equal? (length docs) 4)
     (for ([doc (in-list docs)])
       (check-pred document? doc)))))

(test-case "get-collection: #:limit applies after draft filtering"
  (call-with-site-info
   (λ ()
     (define blog-pages (get-collection "blog" #:limit 2))
     (define titles (map page-link-title blog-pages))
     (check-equal? (length blog-pages) 2)
     (for ([title (in-list titles)])
       (check-not-equal? title "Draft Post")))))

;; ===========================================================================
;; get-taxonomy-terms tests
;; ===========================================================================

(test-case "get-taxonomy-terms: returns distinct terms"
  (call-with-site-info
   (λ ()
     (define terms (get-taxonomy-terms "blog" "tags"))
     (check-pred list? terms)
     ;; Should have alpha, beta, gamma from test fixtures
     (check-not-false (member "alpha" terms) "should include alpha")
     (check-not-false (member "beta" terms) "should include beta")
     (check-not-false (member "gamma" terms) "should include gamma"))))

(test-case "get-taxonomy-terms: terms are ordered by first appearance"
  (call-with-site-info
   (λ ()
     (define terms (get-taxonomy-terms "blog" "tags"))
     ;; Blog order is descending: custom-slug (alpha, gamma), second-post (beta, gamma), first-post (alpha, beta)
     ;; First appearance order: alpha, gamma, beta
     (check-equal? terms '("alpha" "gamma" "beta")))))

(test-case "get-taxonomy-terms: errors when not in build context"
  (check-exn
   exn:fail?
   (λ () (get-taxonomy-terms "blog" "tags"))))

;; ===========================================================================
;; get-taxonomy-pages tests
;; ===========================================================================

(test-case "get-taxonomy-pages: 2-arg returns hash of all terms"
  (call-with-site-info
   (λ ()
     (define all-tags (get-taxonomy-pages "blog" "tags"))
     (check-pred hash? all-tags)
     (check-true (hash-has-key? all-tags "alpha"))
     (check-true (hash-has-key? all-tags "beta"))
     (check-true (hash-has-key? all-tags "gamma")))))

(test-case "get-taxonomy-pages: 3-arg returns page-links for term"
  (call-with-site-info
   (λ ()
     (define alpha-pages (get-taxonomy-pages "blog" "tags" "alpha"))
     (check-pred list? alpha-pages)
     (check-equal? (length alpha-pages) 3)  ; draft-post, custom-slug, first-post
     (for ([pl (in-list alpha-pages)])
       (check-pred page-link? pl)))))

(test-case "get-taxonomy-pages: pages within term are in collection order"
  (call-with-site-info
   (λ ()
     (define alpha-pages (get-taxonomy-pages "blog" "tags" "alpha"))
     (define titles (map page-link-title alpha-pages))
     ;; alpha appears in draft-post, custom-slug (Third Post), first-post
     ;; Descending order: Draft Post (Apr), Third Post (Mar), First Post (Jan)
     (check-equal? titles '("Draft Post" "Third Post" "First Post")))))

(test-case "get-taxonomy-pages: returns empty list for unknown term"
  (call-with-site-info
   (λ ()
     (define pages (get-taxonomy-pages "blog" "tags" "nonexistent"))
     (check-equal? pages '()))))

(test-case "get-taxonomy-pages: errors for unknown collection"
  (call-with-site-info
   (λ ()
     (check-exn
      exn:fail?
      (λ () (get-taxonomy-pages "nonexistent" "tags"))))))

(test-case "get-taxonomy-pages: errors for unknown taxonomy"
  (call-with-site-info
   (λ ()
     (check-exn
      exn:fail?
      (λ () (get-taxonomy-pages "blog" "nonexistent"))))))

(test-case "get-taxonomy-pages: errors when not in build context"
  (check-exn
   exn:fail?
   (λ () (get-taxonomy-pages "blog" "tags"))))

;; ===========================================================================
;; Integration: using retrieval API together
;; ===========================================================================

(test-case "integration: can iterate taxonomy terms and get pages for each"
  (call-with-site-info
   (λ ()
     (define terms (get-taxonomy-terms "blog" "tags"))
     (for ([term (in-list terms)])
       (define pages (get-taxonomy-pages "blog" "tags" term))
       (check-pred list? pages)
       (check-true (>= (length pages) 1))))))
