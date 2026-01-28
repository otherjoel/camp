#lang racket/base

;; Tests for prev/next navigation functions
;;
;; Tests the convenience functions prev and next that extract
;; adjacent pages from a render context.

(require rackunit
         racket/list
         racket/path
         (except-in camp prev next)  ; exclude contracted versions
         camp/build
         (only-in camp/private/main prev next)  ; without contracts for mock testing
         (only-in camp/private/build build-context))

;; ---------------------------------------------------------------------------
;; Test fixture paths

(define fixture-site-root
  (simplify-path
   (build-path (path-only (syntax-source #'here))
               "fixtures" "test-site")))

(define fixture-site-path
  (build-path fixture-site-root "site.rkt"))

;; ---------------------------------------------------------------------------
;; Helper: create a mock context with given prev/next procedures

(define (make-mock-context #:prev [prev-proc (λ args #f)]
                           #:next [next-proc (λ args #f)])
  (hasheq 'slug "test-slug"
          'url "/test/"
          'collection "test"
          'prev prev-proc
          'next next-proc
          'taxonomies (hasheq)))

;; ---------------------------------------------------------------------------
;; prev tests (using private module to test argument passing without contract)

(test-case "prev: calls context's prev procedure with no args"
  (define called-with #f)
  (define mock-page (page-link "/test/" "Test" (hasheq 'slug "test")))
  (define ctx (make-mock-context
               #:prev (λ args (set! called-with args) mock-page)))
  (define result (prev ctx))
  (check-equal? called-with '())
  (check-equal? result mock-page))

(test-case "prev: passes taxonomy key when provided"
  (define called-with #f)
  (define mock-page (page-link "/test/" "Test" (hasheq 'slug "test")))
  (define ctx (make-mock-context
               #:prev (λ args (set! called-with args) mock-page)))
  (define result (prev ctx "tags"))
  (check-equal? called-with '("tags"))
  (check-equal? result mock-page))

(test-case "prev: passes taxonomy key and term when provided"
  (define called-with #f)
  (define mock-page (page-link "/test/" "Test" (hasheq 'slug "test")))
  (define ctx (make-mock-context
               #:prev (λ args (set! called-with args) mock-page)))
  (define result (prev ctx "tags" "emacs"))
  (check-equal? called-with '("tags" "emacs"))
  (check-equal? result mock-page))

(test-case "prev: returns #f when context procedure returns #f"
  (define ctx (make-mock-context #:prev (λ args #f)))
  (check-false (prev ctx)))

;; ---------------------------------------------------------------------------
;; next tests

(test-case "next: calls context's next procedure with no args"
  (define called-with #f)
  (define mock-page (page-link "/test/" "Test" (hasheq 'slug "test")))
  (define ctx (make-mock-context
               #:next (λ args (set! called-with args) mock-page)))
  (define result (next ctx))
  (check-equal? called-with '())
  (check-equal? result mock-page))

(test-case "next: passes taxonomy key when provided"
  (define called-with #f)
  (define mock-page (page-link "/test/" "Test" (hasheq 'slug "test")))
  (define ctx (make-mock-context
               #:next (λ args (set! called-with args) mock-page)))
  (define result (next ctx "tags"))
  (check-equal? called-with '("tags"))
  (check-equal? result mock-page))

(test-case "next: passes taxonomy key and term when provided"
  (define called-with #f)
  (define mock-page (page-link "/test/" "Test" (hasheq 'slug "test")))
  (define ctx (make-mock-context
               #:next (λ args (set! called-with args) mock-page)))
  (define result (next ctx "tags" "emacs"))
  (check-equal? called-with '("tags" "emacs"))
  (check-equal? result mock-page))

(test-case "next: returns #f when context procedure returns #f"
  (define ctx (make-mock-context #:next (λ args #f)))
  (check-false (next ctx)))

;; ---------------------------------------------------------------------------
;; Integration tests with real site fixture

(define (call-with-built-context target-slug thunk)
  ;; Build the site and find the context for the given page
  (define site (load-site fixture-site-path))
  (define info (collect site))
  (parameterize ([current-site-info info])
    ;; Find the page and build its context
    (define pg
      (for/or ([p (site-info-pages info)])
        (and (equal? (page-slug p) target-slug) p)))
    (unless pg
      (error 'call-with-built-context "page not found: ~a" target-slug))
    ;; Build context using internal functions (for testing)
    (define coll-name (page-collection-name pg))
    (define taxonomy-index (site-info-taxonomy-index info))
    (define page-links-by-coll (site-info-page-links-by-collection info))
    (define ctx (build-context pg coll-name taxonomy-index page-links-by-coll))
    (thunk ctx)))

(test-case "integration: prev returns previous page in collection"
  (call-with-built-context "second-post"
    (λ (ctx)
      ;; Blog is descending by date: draft, third (custom-slug), second, first
      ;; So prev of second-post should be custom-slug (Third Post)
      (define p (prev ctx))
      (check-pred page-link? p)
      (check-equal? (page-link-title p) "Third Post"))))

(test-case "integration: next returns next page in collection"
  (call-with-built-context "second-post"
    (λ (ctx)
      ;; next of second-post should be first-post
      (define n (next ctx))
      (check-pred page-link? n)
      (check-equal? (page-link-title n) "First Post"))))

(test-case "integration: prev returns #f for first page in collection"
  (call-with-built-context "draft-post"
    (λ (ctx)
      ;; draft-post is first in descending order
      (check-false (prev ctx)))))

(test-case "integration: next returns #f for last page in collection"
  (call-with-built-context "first-post"
    (λ (ctx)
      ;; first-post is last in descending order
      (check-false (next ctx)))))

(test-case "integration: prev with taxonomy key navigates within taxonomy"
  (call-with-built-context "first-post"
    (λ (ctx)
      ;; first-post has tags: alpha, beta (first tag is alpha)
      ;; Within "alpha" tag (descending): draft-post, custom-slug, first-post
      ;; So prev within alpha should be custom-slug (Third Post)
      (define p (prev ctx "tags"))
      (check-pred page-link? p)
      (check-equal? (page-link-title p) "Third Post"))))

(test-case "integration: next with taxonomy key navigates within taxonomy"
  (call-with-built-context "second-post"
    (λ (ctx)
      ;; second-post has tags: beta, gamma (first tag is beta)
      ;; Within "beta" tag (descending): second-post, first-post
      ;; So next within beta should be first-post
      (define n (next ctx "tags"))
      (check-pred page-link? n)
      (check-equal? (page-link-title n) "First Post"))))
