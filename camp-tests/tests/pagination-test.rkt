#lang racket/base

;; Tests for pagination functionality
;;
;; Tests:
;; - pagination struct and accessors
;; - paginated-content struct (created by paginate form)
;; - page-link-doc function
;; - pagination-nav helper
;; - Build integration for paginated pages

(require rackunit
         racket/list
         racket/path
         racket/file
         compiler/cm
         punct/doc
         punct/fetch  ; for meta-ref
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
;; pagination struct tests
;; ===========================================================================

(test-case "pagination: struct accessors work correctly"
  (define p (pagination 1 5 42 "/blog/" "/blog/" #f "/blog/page/2/"))
  (check-equal? (pagination-page-num p) 1)
  (check-equal? (pagination-total-pages p) 5)
  (check-equal? (pagination-total-items p) 42)
  (check-equal? (pagination-base-url p) "/blog/")
  (check-equal? (pagination-current-url p) "/blog/")
  (check-equal? (pagination-prev-url p) #f)
  (check-equal? (pagination-next-url p) "/blog/page/2/"))

(test-case "pagination: struct is transparent"
  (define p (pagination 2 5 42 "/blog/" "/blog/page/2/" "/blog/" "/blog/page/3/"))
  (check-pred pagination? p)
  (check-equal? (pagination-prev-url p) "/blog/")
  (check-equal? (pagination-next-url p) "/blog/page/3/"))

;; ===========================================================================
;; paginated-content and paginate tests
;; ===========================================================================

(test-case "paginate: creates paginated-content struct"
  (define pc (paginate "blog" #:per-page 10 (λ (items pag) '(div))))
  (check-pred paginated-content? pc))

(test-case "paginate: stores collection name and per-page"
  (define pc (paginate "blog" #:per-page 5 (λ (items pag) '(div))))
  (check-equal? (paginated-content-collection-name pc) "blog")
  (check-equal? (paginated-content-per-page pc) 5))

(test-case "paginate: default page-slug is 'page'"
  (define pc (paginate "blog" #:per-page 10 (λ (items pag) '(div))))
  (check-equal? (paginated-content-page-slug pc) "page"))

(test-case "paginate: custom page-slug"
  (define pc (paginate "blog" #:per-page 10 #:page-slug "p" (λ (items pag) '(div))))
  (check-equal? (paginated-content-page-slug pc) "p"))

(test-case "paginate: stores render procedure"
  (define my-proc (λ (items pag) `(div ,@(map page-link-title items))))
  (define pc (paginate "blog" #:per-page 10 my-proc))
  (check-equal? (paginated-content-render-proc pc) my-proc))

;; ===========================================================================
;; page-link-doc tests
;; ===========================================================================

(test-case "page-link-doc: retrieves document for page-link"
  (call-with-site-info
   (λ ()
     (define blog-pages (get-collection "blog"))
     (define first-page (last blog-pages))  ; first-post is last in descending order
     (define doc (page-link-doc first-page))
     (check-pred document? doc)
     (check-equal? (meta-ref doc 'title) "First Post"))))

(test-case "page-link-doc: works with any page-link from collection"
  (call-with-site-info
   (λ ()
     (define blog-pages (get-collection "blog"))
     (for ([pl (in-list blog-pages)])
       (define doc (page-link-doc pl))
       (check-pred document? doc)
       (check-equal? (meta-ref doc 'title) (page-link-title pl))))))

(test-case "page-link-doc: errors when not in build context"
  (check-exn
   exn:fail?
   (λ ()
     ;; Create a fake page-link outside of build context
     (define fake-pl (page-link "/test/" "Test" (hasheq 'slug "test")))
     (page-link-doc fake-pl))))

;; ===========================================================================
;; pagination-nav tests
;; ===========================================================================

(test-case "pagination-nav: returns empty for single page by default"
  (define p (pagination 1 1 5 "/blog/" "/blog/" #f #f))
  (check-equal? (pagination-nav p) '()))

(test-case "pagination-nav: returns nav for multiple pages"
  (define p (pagination 1 3 25 "/blog/" "/blog/" #f "/blog/page/2/"))
  (define nav (pagination-nav p))
  (check-pred pair? nav)
  (check-equal? (car nav) 'nav))

(test-case "pagination-nav: includes next link on first page"
  (define p (pagination 1 3 25 "/blog/" "/blog/" #f "/blog/page/2/"))
  (define nav (pagination-nav p))
  ;; Should contain a link to page 2
  (check-not-false (member "/blog/page/2/" (flatten nav))))

(test-case "pagination-nav: includes prev and next links on middle page"
  (define p (pagination 2 3 25 "/blog/" "/blog/page/2/" "/blog/" "/blog/page/3/"))
  (define nav (pagination-nav p))
  ;; Should contain links to page 1 and page 3
  (check-not-false (member "/blog/" (flatten nav)))
  (check-not-false (member "/blog/page/3/" (flatten nav))))

(test-case "pagination-nav: includes prev link on last page"
  (define p (pagination 3 3 25 "/blog/" "/blog/page/3/" "/blog/page/2/" #f))
  (define nav (pagination-nav p))
  ;; Should contain link to page 2
  (check-not-false (member "/blog/page/2/" (flatten nav))))

(test-case "pagination-nav: #:always-show? shows nav for single page"
  (define p (pagination 1 1 5 "/blog/" "/blog/" #f #f))
  (define nav (pagination-nav p #:always-show? #t))
  (check-pred pair? nav)
  (check-equal? (car nav) 'nav))

(test-case "pagination-nav: displays current page and total"
  (define p (pagination 2 5 42 "/blog/" "/blog/page/2/" "/blog/" "/blog/page/3/"))
  (define nav (pagination-nav p))
  (define nav-str (format "~a" nav))
  ;; Should contain "2" and "5" somewhere in the output
  (check-regexp-match #rx"2" nav-str)
  (check-regexp-match #rx"5" nav-str))

;; ===========================================================================
;; Build integration tests
;; ===========================================================================

;; Helper: write a paginated archive page
(define (write-paginated-page pages-dir)
  (define archive-path (build-path pages-dir "archive.md.rkt"))
  (call-with-output-file archive-path
    (λ (out)
      (displayln "#lang camp/page" out)
      (newline out)
      (displayln "#:title \"Archive\"" out)
      (displayln "#:output-path \"/archive/\"" out)
      (newline out)
      (displayln "(paginate \"blog\" #:per-page 2" out)
      (displayln "  (λ (items pag)" out)
      (displayln "    `(main" out)
      (displayln "      (h1 \"Archive\")" out)
      (displayln "      (ul ,@(for/list ([p items])" out)
      (displayln "             `(li ,(page-link-title p))))" out)
      (displayln "      ,(pagination-nav pag))))" out))))

;; Helper: compile source files in a directory
(define (compile-sources-in-dir dir)
  (when (directory-exists? dir)
    (for ([p (in-list (directory-list dir #:build? #t))])
      (when (and (file-exists? p)
                 (regexp-match? #rx"\\.rkt$" (path->string p)))
        (managed-compile-zo p)))))

;; Helper to create a temporary test site with pagination
(define (call-with-pagination-test-site thunk)
  (define temp-dir (make-temporary-file "camp-pagination-test-~a" 'directory))
  (dynamic-wind
    void
    (λ ()
      ;; Create site structure
      (define site-rkt (build-path temp-dir "site.rkt"))
      (define pages-dir (build-path temp-dir "pages"))
      (define blog-dir (build-path temp-dir "blog"))
      (define output-dir (build-path temp-dir "publish"))

      (make-directory* pages-dir)
      (make-directory* blog-dir)

      ;; Write site.rkt (using test fixtures render module)
      (call-with-output-file site-rkt
        (λ (out)
          (displayln "#lang camp/site" out)
          (newline out)
          (displayln "title = \"Pagination Test Site\"" out)
          (displayln "url = \"https://test.example.com\"" out)
          (displayln "founded = 2024-01-01" out)
          (displayln "authors = [\"Test (test@example.com)\"]" out)
          (displayln "default-render = '(camp/tests/fixtures/render render-page)'" out)
          (newline out)
          (displayln "[[collections]]" out)
          (displayln "name = \"blog\"" out)
          (displayln "source = \"blog/*\"" out)
          (displayln "output-paths = \"blog/*\"" out)
          (displayln "order = \"descending\"" out)
          (displayln "sort-key = \"date\"" out)
          (newline out)
          (displayln "[[collections]]" out)
          (displayln "name = \"pages\"" out)
          (displayln "source = \"pages/*\"" out)
          (displayln "output-paths = \"*/\"" out)
          (displayln "sort-key = \"title\"" out)
          (displayln "order = \"ascending\"" out)))

      ;; Write blog posts (5 posts for pagination testing)
      (for ([i (in-range 1 6)])
        (define post-path (build-path blog-dir (format "post-~a.md.rkt" i)))
        (call-with-output-file post-path
          (λ (out)
            (displayln "#lang punct" out)
            (displayln "---" out)
            (fprintf out "title: Post ~a~n" i)
            (fprintf out "date: 2024-01-~a~n" (+ 10 i))
            (displayln "---" out)
            (fprintf out "Content for post ~a.~n" i))))

      (thunk temp-dir site-rkt output-dir))
    (λ ()
      (delete-directory/files temp-dir))))

;; Note: Full build integration tests require the paginate functionality
;; to be implemented. These tests document expected behavior.

(test-case "build-integration: paginated page generates multiple output files"
  (call-with-pagination-test-site
   (λ (temp-dir site-rkt output-dir)
     (define pages-dir (build-path temp-dir "pages"))
     (define blog-dir (build-path temp-dir "blog"))
     (write-paginated-page pages-dir)

     ;; Compile sources before build
     (compile-sources-in-dir pages-dir)
     (compile-sources-in-dir blog-dir)

     ;; Build the site
     (define site (load-site site-rkt))
     (define info (collect site))
     (build! site info)

     ;; Check that multiple archive pages were created
     ;; With 5 posts and per-page=2, we expect 3 pages
     (check-true (file-exists? (build-path output-dir "archive" "index.html"))
                 "page 1 should exist")
     (check-true (file-exists? (build-path output-dir "archive" "page" "2" "index.html"))
                 "page 2 should exist")
     (check-true (file-exists? (build-path output-dir "archive" "page" "3" "index.html"))
                 "page 3 should exist")
     (check-false (file-exists? (build-path output-dir "archive" "page" "4" "index.html"))
                  "page 4 should not exist"))))

(test-case "build-integration: paginated pages have correct content per page"
  (call-with-pagination-test-site
   (λ (temp-dir site-rkt output-dir)
     (define pages-dir (build-path temp-dir "pages"))
     (define blog-dir (build-path temp-dir "blog"))
     (write-paginated-page pages-dir)

     ;; Compile sources before build
     (compile-sources-in-dir pages-dir)
     (compile-sources-in-dir blog-dir)

     (define site (load-site site-rkt))
     (define info (collect site))
     (build! site info)

     ;; Read page 1 - should show "Page 1 of 3" and posts 5, 4
     (define page1-content (file->string (build-path output-dir "archive" "index.html")))
     (check-regexp-match #rx"Page 1" page1-content)
     (check-regexp-match #rx"of 3" page1-content)
     (check-regexp-match #rx"Post 5" page1-content)
     (check-regexp-match #rx"Post 4" page1-content)

     ;; Read page 2 - should show "Page 2 of 3" and posts 3, 2
     (define page2-content (file->string (build-path output-dir "archive" "page" "2" "index.html")))
     (check-regexp-match #rx"Page 2" page2-content)
     (check-regexp-match #rx"of 3" page2-content)
     (check-regexp-match #rx"Post 3" page2-content)
     (check-regexp-match #rx"Post 2" page2-content)

     ;; Read page 3 - should show "Page 3 of 3" and post 1
     (define page3-content (file->string (build-path output-dir "archive" "page" "3" "index.html")))
     (check-regexp-match #rx"Page 3" page3-content)
     (check-regexp-match #rx"of 3" page3-content)
     (check-regexp-match #rx"Post 1" page3-content))))

(test-case "build-integration: custom page-slug changes URL structure"
  (call-with-pagination-test-site
   (λ (temp-dir site-rkt output-dir)
     (define pages-dir (build-path temp-dir "pages"))
     (define blog-dir (build-path temp-dir "blog"))

     ;; Write archive with custom page-slug
     (define archive-path (build-path pages-dir "archive.md.rkt"))
     (call-with-output-file archive-path
       (λ (out)
         (displayln "#lang camp/page" out)
         (newline out)
         (displayln "#:title \"Archive\"" out)
         (displayln "#:output-path \"/archive/\"" out)
         (newline out)
         (displayln "(paginate \"blog\" #:per-page 2 #:page-slug \"p\"" out)
         (displayln "  (λ (items pag)" out)
         (displayln "    `(main (ul ,@(for/list ([p items]) `(li ,(page-link-title p)))))))" out)))

     ;; Compile sources before build
     (compile-sources-in-dir pages-dir)
     (compile-sources-in-dir blog-dir)

     (define site (load-site site-rkt))
     (define info (collect site))
     (build! site info)

     ;; Check that pages use custom slug "p" instead of "page"
     (check-true (file-exists? (build-path output-dir "archive" "p" "2" "index.html"))
                 "page 2 should use custom slug 'p'")
     (check-true (file-exists? (build-path output-dir "archive" "p" "3" "index.html"))
                 "page 3 should use custom slug 'p'"))))

(test-case "build-integration: empty collection generates one page"
  (call-with-pagination-test-site
   (λ (temp-dir site-rkt output-dir)
     (define pages-dir (build-path temp-dir "pages"))

     ;; Rewrite site.rkt with an empty collection
     (call-with-output-file site-rkt #:exists 'replace
       (λ (out)
         (displayln "#lang camp/site" out)
         (newline out)
         (displayln "title = \"Pagination Test Site\"" out)
         (displayln "url = \"https://test.example.com\"" out)
         (displayln "founded = 2024-01-01" out)
         (displayln "authors = [\"Test (test@example.com)\"]" out)
         (displayln "default-render = '(camp/tests/fixtures/render render-page)'" out)
         (newline out)
         (displayln "[[collections]]" out)
         (displayln "name = \"empty\"" out)
         (displayln "source = \"empty/*\"" out)
         (displayln "output-paths = \"empty/*\"" out)
         (newline out)
         (displayln "[[collections]]" out)
         (displayln "name = \"pages\"" out)
         (displayln "source = \"pages/*\"" out)
         (displayln "output-paths = \"*/\"" out)
         (displayln "sort-key = \"title\"" out)
         (displayln "order = \"ascending\"" out)))

     ;; Create empty collection directory
     (make-directory* (build-path temp-dir "empty"))

     ;; Write archive that paginates the empty collection
     (define archive-path (build-path pages-dir "archive.md.rkt"))
     (call-with-output-file archive-path
       (λ (out)
         (displayln "#lang camp/page" out)
         (newline out)
         (displayln "#:title \"Empty Archive\"" out)
         (displayln "#:output-path \"/archive/\"" out)
         (newline out)
         (displayln "(paginate \"empty\" #:per-page 2" out)
         (displayln "  (λ (items pag)" out)
         (displayln "    `(main (p \"No posts yet.\"))))" out)))

     ;; Compile sources before build
     (compile-sources-in-dir pages-dir)

     (define site (load-site site-rkt))
     (define info (collect site))
     (build! site info)

     ;; Should generate exactly one page
     (check-true (file-exists? (build-path output-dir "archive" "index.html"))
                 "page 1 should exist even for empty collection")
     (check-false (file-exists? (build-path output-dir "archive" "page" "2" "index.html"))
                  "page 2 should not exist for empty collection"))))

(test-case "build-integration: pagination context has correct URLs"
  (call-with-pagination-test-site
   (λ (temp-dir site-rkt output-dir)
     (define pages-dir (build-path temp-dir "pages"))
     (define blog-dir (build-path temp-dir "blog"))

     ;; Write archive that outputs pagination URLs for verification
     (define archive-path (build-path pages-dir "archive.md.rkt"))
     (call-with-output-file archive-path
       (λ (out)
         (displayln "#lang camp/page" out)
         (newline out)
         (displayln "#:title \"Archive\"" out)
         (displayln "#:output-path \"/archive/\"" out)
         (newline out)
         (displayln "(paginate \"blog\" #:per-page 2" out)
         (displayln "  (λ (items pag)" out)
         (displayln "    `(main" out)
         (displayln "      (p ([id \"base\"]) ,(pagination-base-url pag))" out)
         (displayln "      (p ([id \"current\"]) ,(pagination-current-url pag))" out)
         (displayln "      (p ([id \"prev\"]) ,(or (pagination-prev-url pag) \"none\"))" out)
         (displayln "      (p ([id \"next\"]) ,(or (pagination-next-url pag) \"none\")))))" out)))

     ;; Compile sources before build
     (compile-sources-in-dir pages-dir)
     (compile-sources-in-dir blog-dir)

     (define site (load-site site-rkt))
     (define info (collect site))
     (build! site info)

     ;; Check page 1 URLs
     (define page1-content (file->string (build-path output-dir "archive" "index.html")))
     (check-regexp-match #rx"id=\"base\">/archive/" page1-content)
     (check-regexp-match #rx"id=\"current\">/archive/" page1-content)
     (check-regexp-match #rx"id=\"prev\">none" page1-content)
     (check-regexp-match #rx"id=\"next\">/archive/page/2/" page1-content)

     ;; Check page 2 URLs
     (define page2-content (file->string (build-path output-dir "archive" "page" "2" "index.html")))
     (check-regexp-match #rx"id=\"base\">/archive/" page2-content)
     (check-regexp-match #rx"id=\"current\">/archive/page/2/" page2-content)
     (check-regexp-match #rx"id=\"prev\">/archive/" page2-content)
     (check-regexp-match #rx"id=\"next\">/archive/page/3/" page2-content)

     ;; Check page 3 URLs (last page)
     (define page3-content (file->string (build-path output-dir "archive" "page" "3" "index.html")))
     (check-regexp-match #rx"id=\"prev\">/archive/page/2/" page3-content)
     (check-regexp-match #rx"id=\"next\">none" page3-content))))

;; ===========================================================================
;; Module test runner
;; ===========================================================================

(module+ main
  (require rackunit/text-ui)
  (run-tests (test-suite "Pagination tests")))

(module+ test
  (require rackunit/text-ui))
