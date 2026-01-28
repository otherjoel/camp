#lang racket/base

;; camp/page - A lang for structural/organizational pages

(require (for-syntax racket/base
                     syntax/kerncase)
         (only-in racket/base [#%module-begin base-module-begin])
         camp        ; Re-export camp bindings so they're available in body forms
         punct/doc)  ; Re-export for document struct

(provide (except-out (all-from-out racket/base) #%module-begin date date?)
         (rename-out [camp-page-module-begin #%module-begin])
         (all-from-out camp)
         (all-from-out punct/doc)
         camp-page-doc?)

(define (camp-page-doc? doc)
  (and (document? doc)
       (procedure? (hash-ref (document-metas doc) 'camp-page-body-thunk #f))))

;; ---------------------------------------------------------------------------
;; Form Classification Helpers

(begin-for-syntax
  (require syntax/kerncase)

  (define stop-list
    (append (kernel-form-identifier-list)
            (syntax->list #'(require provide))))

  (define (id-matches? id cmp-ids)
    (and (identifier? id)
         (ormap (λ (cmp) (free-identifier=? id cmp)) cmp-ids)))

  (define lift-always-ids
    (syntax->list #'(require provide #%require #%provide
                             begin-for-syntax module module*)))

  (define definition-ids
    (syntax->list #'(define-values define-syntaxes)))

  (define (form-head-id form)
    (syntax-case form ()
      [(head . _) (identifier? #'head) #'head]
      [_ #f])))

;; ---------------------------------------------------------------------------
;; Module Begin

(define-syntax (camp-page-module-begin stx)
  (syntax-case stx ()
    [(_ form ...)
     (let ([here-path (let ([src (syntax-source stx)])
                        (cond
                          [(path? src) (path->string src)]
                          [(string? src) src]
                          [else "unknown"]))])
       (with-syntax ([here here-path])
         #'(base-module-begin
            (camp-page-process here #f () () form ...))))]))

;; ---------------------------------------------------------------------------
;; Recursive Form Processor

(define-syntax (camp-page-process stx)
  (syntax-case stx ()
    [(_ here_path in-body? ((key val) ...) (body-expr ...))
     #'(begin
         (provide doc)
         (define body-thunk
           (λ () (let () body-expr ...)))
         (define doc
           (document
            (for/fold ([h (hasheq 'here-path here_path
                                  'camp-page-body-thunk body-thunk)])
                      ([k (in-list (list (quote key) ...))]
                       [v (in-list (list val ...))])
              (hash-set h k v))
            '()
            '())))]

    [(_ here-path in-body? metas bodies form0 rest-forms ...)
     (let ([current-form #'form0]
           [datum (syntax-e #'form0)])
       (cond
         [(keyword? datum)
          (syntax-case #'(rest-forms ...) ()
            [()
             (raise-syntax-error 'camp/page
                                 (format "metadata keyword ~a has no value" datum)
                                 current-form)]
            [(val more ...)
             (let ([key (datum->syntax current-form (string->symbol (keyword->string datum)))])
               #`(camp-page-process here-path #t
                                    ((#,key val) #,@#'metas)
                                    bodies
                                    more ...))])]

         [else
          (let* ([expanded (local-expand current-form 'module stop-list)]
                 [head (form-head-id expanded)])
            (cond
              [(and head (free-identifier=? head #'begin))
               (syntax-case expanded ()
                 [(_ inner ...)
                  #`(camp-page-process here-path in-body? metas bodies
                                       inner ... rest-forms ...)])]

              [(and head (id-matches? head lift-always-ids))
               #`(begin
                   #,expanded
                   (camp-page-process here-path in-body? metas bodies rest-forms ...))]

              [(and head (id-matches? head definition-ids))
               (if (syntax-e #'in-body?)
                   #`(camp-page-process here-path #t metas
                                        (#,@(syntax->list #'bodies) #,expanded) rest-forms ...)
                   #`(begin
                       #,expanded
                       (camp-page-process here-path #f metas bodies rest-forms ...)))]

              [else
               #`(camp-page-process here-path #t metas
                                    (#,@(syntax->list #'bodies) #,expanded) rest-forms ...)]))]))]))

;; ---------------------------------------------------------------------------
;; Reader Submodule

(module reader syntax/module-reader
  camp/page)
