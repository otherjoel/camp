#lang racket/base

(require rackunit
         rackunit/text-ui
         racket/logging
         punct/doc
         camp/private/html-render
         camp/private/structs
         camp/private/main
         camp/private/xref
         camp/private/log)

;; Helper to create a minimal site-info for testing
(define (make-test-site-info #:term-index [term-index (hash)]
                              #:page-index [page-index (hash)])
  (site-info '()           ; pages
             term-index
             page-index
             (hash)        ; taxonomy-index
             (hash)        ; pages-by-collection
             (hash)        ; page-links-by-collection
             (hash)))      ; page-by-slug

;; Helper macro to run with test site-info
(define-syntax-rule (with-test-indexes term-index page-index body ...)
  (parameterize ([current-site-info (make-test-site-info #:term-index term-index
                                                          #:page-index page-index)])
    body ...))

(define html-render-tests
  (test-suite
   "HTML render tests"

   (test-suite
    "normalize-term-name"

    (test-case "lowercases term names"
      (check-equal? (normalize-term-name "REST") "rest"))

    (test-case "replaces spaces with hyphens"
      (check-equal? (normalize-term-name "hello world") "hello-world"))

    (test-case "combines lowercase and hyphen replacement"
      (check-equal? (normalize-term-name "REST API") "rest-api"))

    (test-case "strips trailing s for plurals"
      (check-equal? (normalize-term-name "APIs") "api"))

    (test-case "converts ies to y"
      (check-equal? (normalize-term-name "libraries") "library"))

    (test-case "preserves ss endings"
      (check-equal? (normalize-term-name "class") "class"))

    (test-case "strips es after ss (sses -> ss)"
      (check-equal? (normalize-term-name "classes") "class"))

    (test-case "handles chromatic scale singular"
      (check-equal? (normalize-term-name "chromatic scale") "chromatic-scale"))

    (test-case "handles chromatic scales plural"
      (check-equal? (normalize-term-name "chromatic scales") "chromatic-scale")))

   (test-suite
    "normalize-slug"

    (test-case "lowercases slugs"
      (check-equal? (normalize-slug "My-Page") "my-page"))

    (test-case "replaces spaces with hyphens"
      (check-equal? (normalize-slug "my page") "my-page"))

    (test-case "replaces special characters with hyphens"
      (check-equal? (normalize-slug "hello_world") "hello-world")
      (check-equal? (normalize-slug "foo.bar") "foo-bar")
      (check-equal? (normalize-slug "a/b/c") "a-b-c"))

    (test-case "collapses multiple special characters"
      (check-equal? (normalize-slug "hello---world") "hello-world")
      (check-equal? (normalize-slug "foo___bar") "foo-bar")
      (check-equal? (normalize-slug "a   b") "a-b"))

    (test-case "trims leading/trailing hyphens"
      (check-equal? (normalize-slug "-hello-") "hello")
      (check-equal? (normalize-slug "---test---") "test"))

    (test-case "combines all normalizations"
      (check-equal? (normalize-slug "My Cool_Page!") "my-cool-page")
      (check-equal? (normalize-slug "API Documentation (v2)") "api-documentation-v2")))

   (test-suite
    "page-ref function"

    (test-case "page-ref normalizes slug in xexpr"
      (check-equal? (page-ref "My-Page")
                    '(page-ref ((slug "my-page")))))

    (test-case "page-ref normalizes spaces to hyphens"
      (check-equal? (page-ref "my page")
                    '(page-ref ((slug "my-page")))))

    (test-case "page-ref normalizes special characters"
      (check-equal? (page-ref "API_Docs (v2)")
                    '(page-ref ((slug "api-docs-v2")))))

    (test-case "page-ref preserves custom content"
      (check-equal? (page-ref "My-Page" "click here")
                    '(page-ref ((slug "my-page")) "click here"))))

   (test-suite
    "render-term"

    (test-case "renders resolved term as anchor"
      (define term-index (hash "rest" "/glossary/#term-rest"))
      (define page-index (hash))
      (define doc (document (hasheq) '((term () "REST")) '()))
      (define result (with-test-indexes term-index page-index
                       (camp-doc->html-xexpr doc)))
      (check-equal? result
                    '((a ((href "/glossary/#term-rest") (class "term-ref")) "REST"))))

    (test-case "renders unresolved term as error marker"
      (define term-index (hash))
      (define page-index (hash))
      (define doc (document (hasheq) '((term () "UNKNOWN")) '()))
      (define result (with-test-indexes term-index page-index
                       (camp-doc->html-xexpr doc)))
      (check-equal? result
                    '((span ((class "unresolved-ref")) "??UNKNOWN??"))))

    (test-case "normalizes term name for lookup"
      (define term-index (hash "rest-api" "/glossary/#term-rest-api"))
      (define page-index (hash))
      (define doc (document (hasheq) '((term () "REST API")) '()))
      (define result (with-test-indexes term-index page-index
                       (camp-doc->html-xexpr doc)))
      (check-equal? result
                    '((a ((href "/glossary/#term-rest-api") (class "term-ref")) "REST API"))))

    (test-case "resolves plural term to singular definition"
      (define term-index (hash "api" "/glossary/#term-api"))
      (define page-index (hash))
      (define doc (document (hasheq) '((term () "APIs")) '()))
      (define result (with-test-indexes term-index page-index
                       (camp-doc->html-xexpr doc)))
      (check-equal? result
                    '((a ((href "/glossary/#term-api") (class "term-ref")) "APIs")))))

   (test-suite
    "render-term-definition"

    (test-case "renders term definition with id anchor"
      (define term-index (hash))
      (define page-index (hash))
      (define doc (document (hasheq)
                            '((term-definition () "REST"))
                            '()))
      (define result (with-test-indexes term-index page-index
                       (camp-doc->html-xexpr doc)))
      (check-equal? result
                    '((dfn ((id "term-rest") (class "term-def"))
                           "REST"))))

    (test-case "normalizes term name for id"
      (define term-index (hash))
      (define page-index (hash))
      (define doc (document (hasheq)
                            '((term-definition () "REST API"))
                            '()))
      (define result (with-test-indexes term-index page-index
                       (camp-doc->html-xexpr doc)))
      (check-equal? result
                    '((dfn ((id "term-rest-api") (class "term-def"))
                           "REST API")))))

   (test-suite
    "render-page-ref"

    (test-case "renders page ref as anchor with page title"
      (define term-index (hash))
      (define page-index (hash "about" (page-link "/about/" "About Us" (hasheq 'slug "about"))))
      (define doc (document (hasheq) '((page-ref ((slug "about")))) '()))
      (define result (with-test-indexes term-index page-index
                       (camp-doc->html-xexpr doc)))
      (check-equal? result
                    '((a ((href "/about/") (class "page-ref")) "About Us"))))

    (test-case "renders page ref with custom link text"
      (define term-index (hash))
      (define page-index (hash "about" (page-link "/about/" "About Us" (hasheq 'slug "about"))))
      (define doc (document (hasheq) '((page-ref ((slug "about")) "learn more")) '()))
      (define result (with-test-indexes term-index page-index
                       (camp-doc->html-xexpr doc)))
      (check-equal? result
                    '((a ((href "/about/") (class "page-ref")) "learn more"))))

    (test-case "renders unresolved page ref as error marker"
      (define term-index (hash))
      (define page-index (hash))
      (define doc (document (hasheq) '((page-ref ((slug "missing")))) '()))
      (define result (with-test-indexes term-index page-index
                       (camp-doc->html-xexpr doc)))
      (check-equal? result
                    '((span ((class "unresolved-ref")) "??missing??"))))

    (test-case "resolves page ref case-insensitively"
      (define term-index (hash))
      ;; Page indexed with normalized key "my-page"
      (define page-index (hash "my-page" (page-link "/my-page/" "My Page" (hasheq 'slug "My-Page"))))
      ;; Reference uses different case - slug attribute already normalized by page-ref
      (define doc (document (hasheq) '((page-ref ((slug "my-page")))) '()))
      (define result (with-test-indexes term-index page-index
                       (camp-doc->html-xexpr doc)))
      (check-equal? result
                    '((a ((href "/my-page/") (class "page-ref")) "My Page"))))

    (test-case "resolves page ref with special characters normalized"
      (define term-index (hash))
      ;; Page with special chars in original slug, indexed by normalized form
      (define page-index (hash "api-docs" (page-link "/api-docs/" "API Docs" (hasheq 'slug "API_Docs"))))
      ;; Reference slug already normalized
      (define doc (document (hasheq) '((page-ref ((slug "api-docs")))) '()))
      (define result (with-test-indexes term-index page-index
                       (camp-doc->html-xexpr doc)))
      (check-equal? result
                    '((a ((href "/api-docs/") (class "page-ref")) "API Docs")))))

   (test-suite
    "element-fallback"

    (test-case "passes unknown elements to element-fallback"
      (define term-index (hash))
      (define page-index (hash))
      (define (custom-fallback tag attrs elems)
        (if (eq? tag 'callout)
            `(aside ((class "callout")) ,@elems)
            #f))
      (define doc (document (hasheq) '((callout () "Important note")) '()))
      (define result (with-test-indexes term-index page-index
                       (camp-doc->html-xexpr doc custom-fallback)))
      (check-equal? result
                    '((aside ((class "callout")) "Important note"))))

    (test-case "uses default-html-tag when element-fallback returns #f"
      (define term-index (hash))
      (define page-index (hash))
      (define (custom-fallback tag attrs elems) #f)
      (define doc (document (hasheq) '((unknown () "content")) '()))
      (define result (with-test-indexes term-index page-index
                       (camp-doc->html-xexpr doc custom-fallback)))
      (check-equal? result
                    '((unknown "content"))))

    (test-case "uses default-html-tag when no element-fallback provided"
      (define term-index (hash))
      (define page-index (hash))
      (define doc (document (hasheq) '((custom-elem ((data "value")) "text")) '()))
      (define result (with-test-indexes term-index page-index
                       (camp-doc->html-xexpr doc)))
      (check-equal? result
                    '((custom-elem ((data "value")) "text")))))

   (test-suite
    "nested content"

    (test-case "term-definition with formatted content extracts text for id"
      ;; When term has nested formatting like (bold "API"), text extraction
      ;; should still work to create the normalized id
      (define term-index (hash))
      (define page-index (hash))
      (define doc (document (hasheq)
                            '((term-definition () (bold "API")))
                            '()))
      (define result (with-test-indexes term-index page-index
                       (camp-doc->html-xexpr doc)))
      (check-equal? result
                    '((dfn ((id "term-api") (class "term-def"))
                           (b "API")))))

    (test-case "term with formatted content extracts text for lookup"
      (define term-index (hash "api" "/glossary/#term-api"))
      (define page-index (hash))
      (define doc (document (hasheq)
                            '((term () (bold "API")))
                            '()))
      (define result (with-test-indexes term-index page-index
                       (camp-doc->html-xexpr doc)))
      (check-equal? result
                    '((a ((href "/glossary/#term-api") (class "term-ref"))
                         (b "API")))))

    (test-case "page-ref with formatted link text"
      (define term-index (hash))
      (define page-index (hash "guide" (page-link "/guide/" "User Guide" (hasheq 'slug "guide"))))
      (define doc (document (hasheq)
                            '((page-ref ((slug "guide")) "the " (bold "complete") " guide"))
                            '()))
      (define result (with-test-indexes term-index page-index
                       (camp-doc->html-xexpr doc)))
      (check-equal? result
                    '((a ((href "/guide/") (class "page-ref"))
                         "the " (b "complete") " guide"))))

    (test-case "paragraph with term definition and term reference"
      ;; In the new model, term-definition displays its content (the term),
      ;; so this test shows a term definition followed by prose containing a term reference
      (define term-index (hash "rest" "/glossary/#term-rest"))
      (define page-index (hash))
      (define doc (document (hasheq)
                            '((paragraph (term-definition () "RESTful")
                                         " APIs follow " (term () "REST") " principles."))
                            '()))
      (define result (with-test-indexes term-index page-index
                       (camp-doc->html-xexpr doc)))
      (check-equal? result
                    '((p (dfn ((id "term-restful") (class "term-def")) "RESTful")
                         " APIs follow "
                         (a ((href "/glossary/#term-rest") (class "term-ref")) "REST")
                         " principles.")))))

   (test-suite
    "multiple xrefs"

    (test-case "multiple term references in one document"
      (define term-index (hash "rest" "/glossary/#term-rest"
                               "api" "/glossary/#term-api"))
      (define page-index (hash))
      (define doc (document (hasheq)
                            '((paragraph "A " (term () "REST") " " (term () "API") " is common."))
                            '()))
      (define result (with-test-indexes term-index page-index
                       (camp-doc->html-xexpr doc)))
      (check-equal? result
                    '((p "A "
                         (a ((href "/glossary/#term-rest") (class "term-ref")) "REST")
                         " "
                         (a ((href "/glossary/#term-api") (class "term-ref")) "API")
                         " is common."))))

    (test-case "multiple page references in one document"
      (define term-index (hash))
      (define page-index (hash "about" (page-link "/about/" "About" (hasheq 'slug "about"))
                               "contact" (page-link "/contact/" "Contact" (hasheq 'slug "contact"))))
      (define doc (document (hasheq)
                            '((paragraph "See " (page-ref ((slug "about"))) " and " (page-ref ((slug "contact"))) "."))
                            '()))
      (define result (with-test-indexes term-index page-index
                       (camp-doc->html-xexpr doc)))
      (check-equal? result
                    '((p "See "
                         (a ((href "/about/") (class "page-ref")) "About")
                         " and "
                         (a ((href "/contact/") (class "page-ref")) "Contact")
                         "."))))

    (test-case "mixed term and page references"
      (define term-index (hash "rest" "/glossary/#term-rest"))
      (define page-index (hash "tutorial" (page-link "/tutorial/" "Tutorial" (hasheq 'slug "tutorial"))))
      (define doc (document (hasheq)
                            '((paragraph "Learn about " (term () "REST") " in our "
                                         (page-ref ((slug "tutorial"))) "."))
                            '()))
      (define result (with-test-indexes term-index page-index
                       (camp-doc->html-xexpr doc)))
      (check-equal? result
                    '((p "Learn about "
                         (a ((href "/glossary/#term-rest") (class "term-ref")) "REST")
                         " in our "
                         (a ((href "/tutorial/") (class "page-ref")) "Tutorial")
                         "."))))

    (test-case "term definition followed by term reference to same term"
      (define term-index (hash "api" "/current-page/#term-api"))
      (define page-index (hash))
      (define doc (document (hasheq)
                            '((paragraph (term-definition () "API")
                                         " is important. Every " (term () "API") " should be documented."))
                            '()))
      (define result (with-test-indexes term-index page-index
                       (camp-doc->html-xexpr doc)))
      (check-equal? result
                    '((p (dfn ((id "term-api") (class "term-def")) "API")
                         " is important. Every "
                         (a ((href "/current-page/#term-api") (class "term-ref")) "API")
                         " should be documented."))))

    (test-case "multiple unresolved references show individual errors"
      (define term-index (hash))
      (define page-index (hash))
      (define doc (document (hasheq)
                            '((paragraph (term () "FOO") " and " (term () "BAR") " are undefined."))
                            '()))
      (define result (with-test-indexes term-index page-index
                       (camp-doc->html-xexpr doc)))
      (check-equal? result
                    '((p (span ((class "unresolved-ref")) "??FOO??")
                         " and "
                         (span ((class "unresolved-ref")) "??BAR??")
                         " are undefined.")))))

   (test-suite
    "warning messages"

    (test-case "unresolved term logs warning with source path"
      (define warnings '())
      (define (collect-warning! vec)
        (set! warnings (cons (vector-ref vec 1) warnings)))
      (define term-index (hash))
      (define page-index (hash))
      (define doc (document (hasheq 'here-path "/test/page.md.rkt")
                            '((term () "UNKNOWN"))
                            '()))
      (with-intercepted-logging
        collect-warning!
        (λ () (with-test-indexes term-index page-index
                (camp-doc->html-xexpr doc)))
        #:logger camp-logger
        'warning)
      (check-equal? (length warnings) 1)
      (check-regexp-match #rx"/test/page.md.rkt" (car warnings))
      (check-regexp-match #rx"unresolved term reference: UNKNOWN" (car warnings)))

    (test-case "unresolved page ref logs warning with source path"
      (define warnings '())
      (define (collect-warning! vec)
        (set! warnings (cons (vector-ref vec 1) warnings)))
      (define term-index (hash))
      (define page-index (hash))
      (define doc (document (hasheq 'here-path "/test/page.md.rkt")
                            '((page-ref ((slug "missing"))))
                            '()))
      (with-intercepted-logging
        collect-warning!
        (λ () (with-test-indexes term-index page-index
                (camp-doc->html-xexpr doc)))
        #:logger camp-logger
        'warning)
      (check-equal? (length warnings) 1)
      (check-regexp-match #rx"/test/page.md.rkt" (car warnings))
      (check-regexp-match #rx"unresolved page reference: missing" (car warnings)))

    (test-case "warning without here-path still works"
      (define warnings '())
      (define (collect-warning! vec)
        (set! warnings (cons (vector-ref vec 1) warnings)))
      (define term-index (hash))
      (define page-index (hash))
      (define doc (document (hasheq) '((term () "UNKNOWN")) '()))
      (with-intercepted-logging
        collect-warning!
        (λ () (with-test-indexes term-index page-index
                (camp-doc->html-xexpr doc)))
        #:logger camp-logger
        'warning)
      (check-equal? (length warnings) 1)
      (check-regexp-match #rx"unresolved term reference: UNKNOWN" (car warnings))))))

(module+ main
  (run-tests html-render-tests))

(module+ test
  (require rackunit/text-ui)
  (run-tests html-render-tests))
