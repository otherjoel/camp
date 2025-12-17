#lang racket/base

;; camp-html-render%
;;
;; Extends punct-html-render% to handle Camp's cross-reference elements:
;; - term: references to term definitions
;; - term-definition: defines a term with an anchor
;; - page-ref: references to other pages by slug

(require racket/class
         racket/match
         punct/render/html
         punct/fetch
         "structs.rkt"
         "xref.rkt"
         "log.rkt")

(provide camp-html-render%
         camp-doc->html-xexpr)

(define camp-html-render%
  (class punct-html-render%
    (init-field term-index      ; hash: normalized-term-name → url#fragment
                page-index      ; hash: slug → page-link
                [element-fallback #f]   ; (or/c #f (-> symbol? list? list? (or/c xexpr? #f)))
                [source-path #f])       ; (or/c path-string? #f) for warning messages

    ;; Helper to extract an attribute value from attrs list
    (define (get-attr attrs key)
      (define pair (assoc key attrs))
      (and pair (cadr pair)))

    ;; -------------------------------------------------------------------------
    ;; Public methods for rendering xref elements
    ;; These can be overridden by subclasses for custom rendering.

    ;; Render a term reference: looks up the term in term-index
    ;; Returns an anchor linking to the term definition, or error marker if unresolved.
    ;; content is the already-rendered child elements (the term text to display).
    (define/public (render-term content)
      (define term-text (content->text content))
      (define normalized (normalize-term-name term-text))
      (define url (hash-ref term-index normalized #f))
      (cond
        [url
         `(a ((href ,url) (class "term-ref")) ,@content)]
        [else
         (if source-path
             (log-camp-warning "~a: unresolved term reference: ~a" source-path term-text)
             (log-camp-warning "unresolved term reference: ~a" term-text))
         `(span ((class "unresolved-ref")) ,(format "??~a??" term-text))]))

    ;; Render a term definition: creates a dfn element with an id anchor.
    ;; content is the already-rendered child elements (the term text to display).
    (define/public (render-term-definition content)
      (define term-text (content->text content))
      (define normalized (normalize-term-name term-text))
      `(dfn ((id ,(string-append "term-" normalized)) (class "term-def")) ,@content))

    ;; Render a page reference: looks up the slug in page-index.
    ;; content is the already-rendered child elements (used as link text if non-empty).
    (define/public (render-page-ref slug content)
      (define pl (hash-ref page-index slug #f))
      (cond
        [pl
         (define url (page-link-url pl))
         (define title (page-link-title pl))
         (define link-text (if (null? content) (list title) content))
         `(a ((href ,url) (class "page-ref")) ,@link-text)]
        [else
         (if source-path
             (log-camp-warning "~a: unresolved page reference: ~a" source-path slug)
             (log-camp-warning "unresolved page reference: ~a" slug))
         `(span ((class "unresolved-ref")) ,(format "??~a??" slug))]))

    ;; -------------------------------------------------------------------------
    ;; Fallback dispatcher

    (define (camp-fallback tag attrs elems)
      (match tag
        ['term
         (render-term elems)]
        ['term-definition
         (render-term-definition elems)]
        ['page-ref
         (render-page-ref (get-attr attrs 'slug) elems)]
        [_
         ;; Try element-fallback first, then default
         (cond
           [(and element-fallback (element-fallback tag attrs elems))
            => values]
           [else
            (default-html-tag tag attrs elems)])]))

    (super-new [render-fallback camp-fallback])))

;; Convenience function to render a document to HTML x-expressions
;; using Camp's cross-reference resolution.
(define (camp-doc->html-xexpr doc term-index page-index [element-fallback #f])
  (define here-path (meta-ref doc 'here-path))
  (send (new camp-html-render%
             [doc doc]
             [term-index term-index]
             [page-index page-index]
             [element-fallback element-fallback]
             [source-path here-path])
        render-document))
