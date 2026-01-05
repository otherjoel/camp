#lang racket/base

;; Camp's Typst renderer extends punct's Typst renderer with:
;; - Automatic label attachment to leading H1 headings (for book cross-references)
;; - page-ref element handling (converts to Typst @ref or #link)
;; - Falls through to default-typst-tag for term/term-definition (no override needed)

(require punct/render/typst
         punct/doc
         racket/class
         racket/format
         racket/match
         racket/string
         "structs.rkt")

(provide camp-typst-render%
         camp-doc->typst
         (all-from-out punct/render/typst))

;; ---------------------------------------------------------------------------
;; Helper: join rendered elements

(define (join-elems elems)
  (string-append* (map ~a elems)))

(define (block v)
  (~a v "\n\n"))

;; ---------------------------------------------------------------------------
;; Camp's Typst renderer class

(define camp-typst-render%
  (class punct-typst-render%
    (init-field [slug #f])
    (inherit-field doc)
    (super-new [render-fallback (make-camp-fallback slug)])

    ;; Flag: should the first H1 we encounter get a label?
    (define first-h1-needs-label? #f)

    (define/override (render-document)
      ;; Check if document starts with H1 and we have a slug
      (define body (document-body doc))
      (when (and slug (pair? body))
        (match (car body)
          [(list 'heading (list (list 'level "1") _ ...) _ ...)
           (set! first-h1-needs-label? #t)]
          [_ (void)]))
      (super render-document))

    (define/override (render-heading level elems)
      (cond
        [(and first-h1-needs-label? (equal? level "1"))
         (set! first-h1-needs-label? #f)
         (block (~a (make-string (string->number level) #\=) " "
                    (join-elems elems) " <" slug ">"))]
        [else (super render-heading level elems)]))))

;; ---------------------------------------------------------------------------
;; Fallback function for custom elements

(define ((make-camp-fallback slug) tag attrs elems)
  (match tag
    ['page-ref
     (define target-slug (get-attr attrs 'slug))
     (if (null? elems)
         ;; No content: use @ref syntax
         (~a "@" target-slug)
         ;; With content: use #link(<label>)[content]
         (~a "#link(<" target-slug ">)[" (join-elems elems) "]"))]
    [_ (default-typst-tag tag attrs elems)]))

(define (get-attr attrs key)
  (define pair (assoc key attrs))
  (and pair (cadr pair)))

;; ---------------------------------------------------------------------------
;; Convenience function matching punct's doc->typst

(define (camp-doc->typst doc #:slug [slug #f])
  (send (new camp-typst-render% [doc doc] [slug slug])
        render-document))

