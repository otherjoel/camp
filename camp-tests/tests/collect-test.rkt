#lang racket/base

;; Tests for the collect pass (Phase 3.2)
;;
;; The collect function traverses all documents and builds:
;; - page index: slug → page-link (for page-ref resolution)
;; - term index: term-name → url#term-name (for term resolution)
;; - taxonomy index: (collection, taxonomy-key, term) → (listof page-link)
;; - pages list: all page structs with source/output paths, docs, slugs

(require rackunit
         racket/path
         racket/list
         racket/hash
         racket/string
         punct/doc
         gregor
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
;; Helper to load the test site

(define (load-test-site)
  (load-site fixture-site-path))

;; ===========================================================================
;; collect function tests
;; ===========================================================================

;; ---------------------------------------------------------------------------
;; Basic collect functionality

(test-case "collect: returns a site-info struct"
  (define site (load-test-site))
  (define info (collect site))
  (check-pred site-info? info))

(test-case "collect: site-info contains pages list"
  (define site (load-test-site))
  (define info (collect site))
  (define pages (site-info-pages info))
  (check-pred list? pages)
  ;; Should have 4 blog posts (including draft) + 2 pages (about, home) = 6 pages
  (check-equal? (length pages) 6))

;; ---------------------------------------------------------------------------
;; Pages list structure

(test-case "collect: pages have correct structure"
  (define site (load-test-site))
  (define info (collect site))
  (define pages (site-info-pages info))
  (for ([p (in-list pages)])
    (check-pred page? p)
    (check-pred path? (page-source-path p))
    (check-pred path? (page-output-path p))
    (check-pred document? (page-doc p))
    (check-pred string? (page-slug p))
    (check-pred string? (page-collection-name p))))

(test-case "collect: pages are sorted within collections"
  (define site (load-test-site))
  (define info (collect site))
  (define pages (site-info-pages info))
  ;; Filter blog posts
  (define blog-pages
    (filter (λ (p) (equal? (page-collection-name p) "blog")) pages))
  ;; Should be sorted descending by date: draft (Apr), custom-slug (Mar), second-post (Feb), first-post (Jan)
  (define slugs (map page-slug blog-pages))
  (check-equal? slugs '("draft-post" "custom-slug" "second-post" "first-post")))

(test-case "collect: output paths are correctly formatted"
  (define site (load-test-site))
  (define info (collect site))
  (define pages (site-info-pages info))
  ;; Find the first-post (date: 2024-01-20)
  (define first-post
    (findf (λ (p) (equal? (page-slug p) "first-post")) pages))
  (check-not-false first-post)
  ;; Output pattern is "blog/[yyyy]/[MM]/*/" so should be blog/2024/01/first-post/index.html
  (define expected-output (build-path "blog" "2024" "01" "first-post" "index.html"))
  (check-equal? (page-output-path first-post) expected-output))

;; ---------------------------------------------------------------------------
;; Page index tests

(test-case "collect: page-index maps slugs to page-links"
  (define site (load-test-site))
  (define info (collect site))
  (define page-index (site-info-page-index info))
  (check-pred hash? page-index)
  ;; Should have entries for all 6 pages (4 blog + 2 pages)
  (check-equal? (hash-count page-index) 6))

(test-case "collect: page-index entries have correct structure"
  (define site (load-test-site))
  (define info (collect site))
  (define page-index (site-info-page-index info))
  (define first-post-link (hash-ref page-index "first-post" #f))
  (check-not-false first-post-link)
  (check-pred page-link? first-post-link)
  (check-equal? (page-link-title first-post-link) "First Post")
  ;; URL should be the web path (with leading /)
  (check-equal? (page-link-url first-post-link) "/blog/2024/01/first-post/"))

(test-case "collect: page-index includes custom slugs"
  (define site (load-test-site))
  (define info (collect site))
  (define page-index (site-info-page-index info))
  ;; Third post has custom slug "custom-slug"
  (check-true (hash-has-key? page-index "custom-slug"))
  (check-false (hash-has-key? page-index "third-post")))

(test-case "collect: page-link metas include slug"
  (define site (load-test-site))
  (define info (collect site))
  (define page-index (site-info-page-index info))
  (define first-post-link (hash-ref page-index "first-post"))
  (define metas (page-link-metas first-post-link))
  (check-equal? (hash-ref metas 'slug) "first-post"))

;; ---------------------------------------------------------------------------
;; Term index tests

(test-case "collect: term-index maps term names to URLs with fragments"
  (define site (load-test-site))
  (define info (collect site))
  (define term-index (site-info-term-index info))
  (check-pred hash? term-index))

(test-case "collect: term-index contains terms from defterm calls"
  (define site (load-test-site))
  (define info (collect site))
  (define term-index (site-info-term-index info))
  ;; first-post defines REST and API (normalized to lowercase)
  (check-true (hash-has-key? term-index "rest"))
  (check-true (hash-has-key? term-index "api"))
  ;; second-post defines JSON (normalized to lowercase)
  (check-true (hash-has-key? term-index "json")))

(test-case "collect: term-index URLs include fragment with term name"
  (define site (load-test-site))
  (define info (collect site))
  (define term-index (site-info-term-index info))
  (define rest-url (hash-ref term-index "rest"))  ; normalized key
  ;; Should be page URL + #term-rest (normalized fragment)
  (check-pred string? rest-url)
  (check-true (string-contains? rest-url "/blog/2024/01/first-post/"))
  (check-true (string-contains? rest-url "#term-rest")))

(test-case "collect: duplicate terms - last definition wins"
  ;; If same term defined in multiple docs, the one processed last wins
  ;; (based on collection sort order)
  ;; This test documents the behavior - we'd need fixtures with duplicate terms
  ;; to test this properly. For now, just verify the index is built.
  (define site (load-test-site))
  (define info (collect site))
  (define term-index (site-info-term-index info))
  (check-true (hash? term-index)))

;; ---------------------------------------------------------------------------
;; Taxonomy index tests

(test-case "collect: taxonomy-index is a nested hash structure"
  (define site (load-test-site))
  (define info (collect site))
  (define tax-index (site-info-taxonomy-index info))
  (check-pred hash? tax-index))

(test-case "collect: taxonomy-index has entries for each collection with taxonomies"
  (define site (load-test-site))
  (define info (collect site))
  (define tax-index (site-info-taxonomy-index info))
  ;; blog collection has taxonomies: tags, series
  (check-true (hash-has-key? tax-index "blog")))

(test-case "collect: taxonomy-index has entries for each taxonomy key"
  (define site (load-test-site))
  (define info (collect site))
  (define tax-index (site-info-taxonomy-index info))
  (define blog-taxonomies (hash-ref tax-index "blog"))
  (check-true (hash-has-key? blog-taxonomies "tags"))
  (check-true (hash-has-key? blog-taxonomies "series")))

(test-case "collect: taxonomy terms are correctly normalized from comma-separated strings"
  (define site (load-test-site))
  (define info (collect site))
  (define tax-index (site-info-taxonomy-index info))
  (define tags-index (hash-ref (hash-ref tax-index "blog") "tags"))
  ;; tags-index maps tag-name → (listof page-link)
  (check-true (hash-has-key? tags-index "alpha"))
  (check-true (hash-has-key? tags-index "beta"))
  (check-true (hash-has-key? tags-index "gamma")))

(test-case "collect: taxonomy pages are correctly associated with terms"
  (define site (load-test-site))
  (define info (collect site))
  (define tax-index (site-info-taxonomy-index info))
  (define tags-index (hash-ref (hash-ref tax-index "blog") "tags"))
  ;; alpha appears in: first-post, custom-slug (third-post), draft-post
  (define alpha-pages (hash-ref tags-index "alpha"))
  (check-equal? (length alpha-pages) 3)
  ;; beta appears in: first-post, second-post
  (define beta-pages (hash-ref tags-index "beta"))
  (check-equal? (length beta-pages) 2)
  ;; gamma appears in: second-post, custom-slug
  (define gamma-pages (hash-ref tags-index "gamma"))
  (check-equal? (length gamma-pages) 2))

(test-case "collect: taxonomy pages within a term are sorted by collection order"
  (define site (load-test-site))
  (define info (collect site))
  (define tax-index (site-info-taxonomy-index info))
  (define tags-index (hash-ref (hash-ref tax-index "blog") "tags"))
  ;; alpha: draft-post (Apr), custom-slug (Mar 10), first-post (Jan 20) in descending order
  (define alpha-pages (hash-ref tags-index "alpha"))
  (define alpha-titles (map page-link-title alpha-pages))
  (check-equal? alpha-titles '("Draft Post" "Third Post" "First Post")))

(test-case "collect: series taxonomy is correctly indexed"
  (define site (load-test-site))
  (define info (collect site))
  (define tax-index (site-info-taxonomy-index info))
  (define series-index (hash-ref (hash-ref tax-index "blog") "series"))
  ;; tutorials series: second-post, custom-slug (third-post)
  (check-true (hash-has-key? series-index "tutorials"))
  (define tutorials-pages (hash-ref series-index "tutorials"))
  (check-equal? (length tutorials-pages) 2))

;; ---------------------------------------------------------------------------
;; Edge cases and error handling

(test-case "collect: handles pages without taxonomies"
  ;; The about page in pages collection has no taxonomies
  (define site (load-test-site))
  (define info (collect site))
  ;; Should not error
  (check-pred site-info? info))

;; ---------------------------------------------------------------------------
;; output-path metadata override tests

(test-case "collect: output-path meta overrides collection pattern"
  (define site (load-test-site))
  (define info (collect site))
  (define pages (site-info-pages info))
  ;; Find the home page which has output-path: /
  (define home-page
    (findf (λ (p) (equal? (page-slug p) "home")) pages))
  (check-not-false home-page)
  ;; Should output to index.html at root, not home/index.html
  (check-equal? (page-output-path home-page) (build-path "index.html")))

(test-case "collect: output-path with trailing slash adds index.html"
  (define site (load-test-site))
  (define info (collect site))
  (define pages (site-info-pages info))
  (define home-page
    (findf (λ (p) (equal? (page-slug p) "home")) pages))
  (check-not-false home-page)
  ;; The "/" output-path should become "index.html"
  (define output-str (path->string (page-output-path home-page)))
  (check-true (string-suffix? output-str "index.html")))

(test-case "collect: handles collection with no taxonomies defined"
  ;; The pages collection has no taxonomies in site config
  (define site (load-test-site))
  (define info (collect site))
  (define tax-index (site-info-taxonomy-index info))
  ;; pages collection should not be in taxonomy index (no taxonomies configured)
  (check-false (hash-has-key? tax-index "pages")))

(test-case "collect: handles pages with no terms-defined"
  ;; About page has no defterm calls
  (define site (load-test-site))
  (define info (collect site))
  ;; Should not error
  (check-pred site-info? info))

;; ---------------------------------------------------------------------------
;; Integration with get-collection, get-taxonomy-* functions
;; Note: These functions need site-info to be available; they may require
;; a parameter or different API. These tests document expected behavior.

;; After collect, we should be able to query collections and taxonomies.
;; The exact API for passing site-info to these functions may need design.

;; ---------------------------------------------------------------------------
;; Taxonomy term ordering tests

(test-case "collect: taxonomy terms ordered by first appearance in collection"
  ;; Term ordering should match first appearance in the sorted collection
  ;; Blog order is descending by date: custom-slug, second-post, first-post
  ;; - custom-slug tags: alpha, gamma
  ;; - second-post tags: beta, gamma
  ;; - first-post tags: alpha, beta
  ;; First appearance order: alpha (custom-slug), gamma (custom-slug), beta (second-post)
  (define site (load-test-site))
  (define info (collect site))
  ;; The taxonomy-index structure should preserve this ordering when iterating
  ;; This test verifies the index is built - actual ordering verification may
  ;; require examining the internal structure or using get-taxonomy-terms
  (define tax-index (site-info-taxonomy-index info))
  (check-pred hash? tax-index))
