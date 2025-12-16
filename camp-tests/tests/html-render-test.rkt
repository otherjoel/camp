#lang racket/base

(require rackunit
         rackunit/text-ui
         punct/doc
         camp/private/html-render
         camp/private/structs
         camp/private/xref)

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
    "render-term"

    (test-case "renders resolved term as anchor"
      (define term-index (hash "rest" "/glossary/#term-rest"))
      (define page-index (hash))
      (define doc (document (hasheq) '((term () "REST")) '()))
      (define result (camp-doc->html-xexpr doc term-index page-index))
      (check-equal? result
                    '(article (a ((href "/glossary/#term-rest") (class "term-ref")) "REST"))))

    (test-case "renders unresolved term as error marker"
      (define term-index (hash))
      (define page-index (hash))
      (define doc (document (hasheq) '((term () "UNKNOWN")) '()))
      (define result (camp-doc->html-xexpr doc term-index page-index))
      (check-equal? result
                    '(article (span ((class "unresolved-ref")) "??UNKNOWN??"))))

    (test-case "normalizes term name for lookup"
      (define term-index (hash "rest-api" "/glossary/#term-rest-api"))
      (define page-index (hash))
      (define doc (document (hasheq) '((term () "REST API")) '()))
      (define result (camp-doc->html-xexpr doc term-index page-index))
      (check-equal? result
                    '(article (a ((href "/glossary/#term-rest-api") (class "term-ref")) "REST API"))))

    (test-case "resolves plural term to singular definition"
      (define term-index (hash "api" "/glossary/#term-api"))
      (define page-index (hash))
      (define doc (document (hasheq) '((term () "APIs")) '()))
      (define result (camp-doc->html-xexpr doc term-index page-index))
      (check-equal? result
                    '(article (a ((href "/glossary/#term-api") (class "term-ref")) "APIs")))))

   (test-suite
    "render-term-definition"

    (test-case "renders term definition with id anchor"
      (define term-index (hash))
      (define page-index (hash))
      (define doc (document (hasheq)
                            '((term-definition () "REST"))
                            '()))
      (define result (camp-doc->html-xexpr doc term-index page-index))
      (check-equal? result
                    '(article (dfn ((id "term-rest") (class "term-def"))
                                   "REST"))))

    (test-case "normalizes term name for id"
      (define term-index (hash))
      (define page-index (hash))
      (define doc (document (hasheq)
                            '((term-definition () "REST API"))
                            '()))
      (define result (camp-doc->html-xexpr doc term-index page-index))
      (check-equal? result
                    '(article (dfn ((id "term-rest-api") (class "term-def"))
                                   "REST API")))))

   (test-suite
    "render-page-ref"

    (test-case "renders page ref as anchor with page title"
      (define term-index (hash))
      (define page-index (hash "about" (page-link "/about/" "About Us" (hasheq 'slug "about"))))
      (define doc (document (hasheq) '((page-ref ((slug "about")))) '()))
      (define result (camp-doc->html-xexpr doc term-index page-index))
      (check-equal? result
                    '(article (a ((href "/about/") (class "page-ref")) "About Us"))))

    (test-case "renders page ref with custom link text"
      (define term-index (hash))
      (define page-index (hash "about" (page-link "/about/" "About Us" (hasheq 'slug "about"))))
      (define doc (document (hasheq) '((page-ref ((slug "about")) "learn more")) '()))
      (define result (camp-doc->html-xexpr doc term-index page-index))
      (check-equal? result
                    '(article (a ((href "/about/") (class "page-ref")) "learn more"))))

    (test-case "renders unresolved page ref as error marker"
      (define term-index (hash))
      (define page-index (hash))
      (define doc (document (hasheq) '((page-ref ((slug "missing")))) '()))
      (define result (camp-doc->html-xexpr doc term-index page-index))
      (check-equal? result
                    '(article (span ((class "unresolved-ref")) "??missing??")))))

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
      (define result (camp-doc->html-xexpr doc term-index page-index custom-fallback))
      (check-equal? result
                    '(article (aside ((class "callout")) "Important note"))))

    (test-case "uses default-html-tag when element-fallback returns #f"
      (define term-index (hash))
      (define page-index (hash))
      (define (custom-fallback tag attrs elems) #f)
      (define doc (document (hasheq) '((unknown () "content")) '()))
      (define result (camp-doc->html-xexpr doc term-index page-index custom-fallback))
      (check-equal? result
                    '(article (unknown "content"))))

    (test-case "uses default-html-tag when no element-fallback provided"
      (define term-index (hash))
      (define page-index (hash))
      (define doc (document (hasheq) '((custom-elem ((data "value")) "text")) '()))
      (define result (camp-doc->html-xexpr doc term-index page-index))
      (check-equal? result
                    '(article (custom-elem ((data "value")) "text")))))

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
      (define result (camp-doc->html-xexpr doc term-index page-index))
      (check-equal? result
                    '(article (dfn ((id "term-api") (class "term-def"))
                                   (b "API")))))

    (test-case "term with formatted content extracts text for lookup"
      (define term-index (hash "api" "/glossary/#term-api"))
      (define page-index (hash))
      (define doc (document (hasheq)
                            '((term () (bold "API")))
                            '()))
      (define result (camp-doc->html-xexpr doc term-index page-index))
      (check-equal? result
                    '(article (a ((href "/glossary/#term-api") (class "term-ref"))
                                 (b "API")))))

    (test-case "page-ref with formatted link text"
      (define term-index (hash))
      (define page-index (hash "guide" (page-link "/guide/" "User Guide" (hasheq 'slug "guide"))))
      (define doc (document (hasheq)
                            '((page-ref ((slug "guide")) "the " (bold "complete") " guide"))
                            '()))
      (define result (camp-doc->html-xexpr doc term-index page-index))
      (check-equal? result
                    '(article (a ((href "/guide/") (class "page-ref"))
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
      (define result (camp-doc->html-xexpr doc term-index page-index))
      (check-equal? result
                    '(article (p (dfn ((id "term-restful") (class "term-def")) "RESTful")
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
      (define result (camp-doc->html-xexpr doc term-index page-index))
      (check-equal? result
                    '(article (p "A "
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
      (define result (camp-doc->html-xexpr doc term-index page-index))
      (check-equal? result
                    '(article (p "See "
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
      (define result (camp-doc->html-xexpr doc term-index page-index))
      (check-equal? result
                    '(article (p "Learn about "
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
      (define result (camp-doc->html-xexpr doc term-index page-index))
      (check-equal? result
                    '(article (p (dfn ((id "term-api") (class "term-def")) "API")
                                 " is important. Every "
                                 (a ((href "/current-page/#term-api") (class "term-ref")) "API")
                                 " should be documented."))))

    (test-case "multiple unresolved references show individual errors"
      (define term-index (hash))
      (define page-index (hash))
      (define doc (document (hasheq)
                            '((paragraph (term () "FOO") " and " (term () "BAR") " are undefined."))
                            '()))
      (define result (camp-doc->html-xexpr doc term-index page-index))
      (check-equal? result
                    '(article (p (span ((class "unresolved-ref")) "??FOO??")
                                 " and "
                                 (span ((class "unresolved-ref")) "??BAR??")
                                 " are undefined.")))))))

(module+ main
  (run-tests html-render-tests))

(module+ test
  (require rackunit/text-ui)
  (run-tests html-render-tests))
