#lang racket/base

;; Extends punct-html-render% for Camp cross-reference elements

(require racket/class
         racket/match
         punct/render/html
         punct/fetch
         "structs.rkt"
         "xref.rkt"
         "log.rkt"
         "main.rkt")

(provide camp-html-render%
         camp-doc->html-xexpr)

(define camp-html-render%
  (class punct-html-render%
    (init-field term-index
                page-index
                [element-fallback #f]
                [source-path #f])

    (define (get-attr attrs key)
      (define pair (assoc key attrs))
      (and pair (cadr pair)))

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

    (define/public (render-term-definition content)
      (define term-text (content->text content))
      (define normalized (normalize-term-name term-text))
      `(dfn ((id ,(string-append "term-" normalized)) (class "term-def")) ,@content))

    (define/public (render-page-ref slug content)
      (define normalized (normalize-slug slug))
      (define pl (hash-ref page-index normalized #f))
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

    (define (camp-fallback tag attrs elems)
      (match tag
        ['term
         (render-term elems)]
        ['term-definition
         (render-term-definition elems)]
        ['page-ref
         (render-page-ref (get-attr attrs 'slug) elems)]
        [_
         (cond
           [(and element-fallback (element-fallback tag attrs elems))
            => values]
           [else
            (default-html-tag tag attrs elems)])]))

    (super-new [render-fallback camp-fallback])))

(define (camp-doc->html-xexpr doc [fallback #f])
  (define body-thunk (meta-ref doc 'camp-page-body-thunk))
  (cond
    [body-thunk
     ;; #lang camp/page document: call thunk to get body xexprs
     (define result (body-thunk))
     (if (and (pair? result) (symbol? (car result)))
         (list result)
         result)]
    [else
     ;; Punct document: render with cross-reference resolution
     (define info (current-site-info))
     (unless info
       (error 'camp-doc->html-xexpr
              "no site-info available; must be called during build or within (parameterize ([current-site-info ...]) ...)"))
     (define here-path (meta-ref doc 'here-path))
     (define rendered
       (send (new camp-html-render%
                  [doc doc]
                  [term-index (site-info-term-index info)]
                  [page-index (site-info-page-index info)]
                  [element-fallback fallback]
                  [source-path here-path])
             render-document))
     ;; Strip outer wrapper, return just body content
     (if (>= (length rendered) 2)
         (cdr rendered)
         '())]))
