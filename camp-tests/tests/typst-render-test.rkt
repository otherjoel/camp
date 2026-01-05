#lang racket/base

;; Tests for camp-typst-render% (Phase 9.5)
;;
;; The typst renderer extends punct's typst renderer with:
;; - Automatic label attachment to leading H1 headings (when slug provided)
;; - page-ref element handling (converts to @slug or #link(<slug>)[text])
;; - Falls through to default-typst-tag for term/term-definition

(require rackunit
         rackunit/text-ui
         punct/doc
         camp/private/typst-render
         camp/private/structs)

(define typst-render-tests
  (test-suite
   "Typst render tests"

   ;; -------------------------------------------------------------------------
   (test-suite
    "H1 label attachment"

    (test-case "attaches slug label to leading H1"
      (define doc (document (hasheq)
                            '((heading ((level "1")) "Introduction")
                              (paragraph "Some text here."))
                            '()))
      (define result (camp-doc->typst doc #:slug "intro"))
      (check-equal? result "= Introduction <intro>\n\nSome text here.\n\n"))

    (test-case "H1 with inline formatting preserves formatting"
      (define doc (document (hasheq)
                            '((heading ((level "1")) "The " (bold "Bold") " Chapter")
                              (paragraph "Body text."))
                            '()))
      (define result (camp-doc->typst doc #:slug "bold-chapter"))
      (check-equal? result "= The *Bold* Chapter <bold-chapter>\n\nBody text.\n\n"))

    (test-case "no label when slug not provided"
      (define doc (document (hasheq)
                            '((heading ((level "1")) "Introduction")
                              (paragraph "Text."))
                            '()))
      (define result (camp-doc->typst doc))
      (check-equal? result "= Introduction\n\nText.\n\n"))

    (test-case "no label attached to H2 even with slug"
      (define doc (document (hasheq)
                            '((heading ((level "2")) "Subsection")
                              (paragraph "Text."))
                            '()))
      (define result (camp-doc->typst doc #:slug "subsection"))
      (check-equal? result "== Subsection\n\nText.\n\n"))

    (test-case "no label when first element is paragraph"
      (define doc (document (hasheq)
                            '((paragraph "Starting with text.")
                              (heading ((level "1")) "Late Heading"))
                            '()))
      (define result (camp-doc->typst doc #:slug "no-label"))
      (check-equal? result "Starting with text.\n\n= Late Heading\n\n"))

    (test-case "empty document with slug produces empty string"
      (define doc (document (hasheq) '() '()))
      (define result (camp-doc->typst doc #:slug "empty"))
      (check-equal? result "")))

   ;; -------------------------------------------------------------------------
   (test-suite
    "page-ref rendering"

    (test-case "page-ref without content becomes @slug"
      (define doc (document (hasheq)
                            '((paragraph "See " (page-ref ((slug "intro"))) "."))
                            '()))
      (define result (camp-doc->typst doc))
      (check-equal? result "See @intro.\n\n"))

    (test-case "page-ref with content becomes #link"
      (define doc (document (hasheq)
                            '((paragraph "See " (page-ref ((slug "intro")) "the intro") "."))
                            '()))
      (define result (camp-doc->typst doc))
      (check-equal? result "See #link(<intro>)[the intro].\n\n"))

    (test-case "page-ref with formatted content"
      (define doc (document (hasheq)
                            '((paragraph (page-ref ((slug "guide")) "the " (bold "complete") " guide")))
                            '()))
      (define result (camp-doc->typst doc))
      (check-equal? result "#link(<guide>)[the *complete* guide]\n\n"))

    (test-case "multiple page-refs in document"
      (define doc (document (hasheq)
                            '((paragraph "See " (page-ref ((slug "ch1"))) " and " (page-ref ((slug "ch2"))) "."))
                            '()))
      (define result (camp-doc->typst doc))
      (check-equal? result "See @ch1 and @ch2.\n\n")))

   ;; -------------------------------------------------------------------------
   (test-suite
    "term and term-definition passthrough"

    (test-case "term without name becomes #term[content]"
      (define doc (document (hasheq)
                            '((paragraph "The " (term () "REST") " protocol."))
                            '()))
      (define result (camp-doc->typst doc))
      (check-equal? result "The #term[REST] protocol.\n\n"))

    (test-case "term-definition with name attribute"
      (define doc (document (hasheq)
                            '((paragraph (term-definition ((name "rest")) "REST") " means..."))
                            '()))
      (define result (camp-doc->typst doc))
      (check-equal? result "#term_definition(name: \"rest\")[REST] means...\n\n"))

    (test-case "term-definition without name attribute"
      (define doc (document (hasheq)
                            '((paragraph (term-definition () "API") " is..."))
                            '()))
      (define result (camp-doc->typst doc))
      (check-equal? result "#term_definition[API] is...\n\n")))

))

(module+ main
  (run-tests typst-render-tests))

(module+ test
  (require rackunit/text-ui)
  (run-tests typst-render-tests))
