#lang racket/base

;; camp/page - A lang for structural/organizational pages
;;
;; Unlike #lang punct, this lang doesn't process content through CommonMark.
;; Instead, body expressions are wrapped in a thunk and evaluated at render
;; time when site-info is available.
;;
;; Uses a recursive macro pattern with local-expand for robust handling of
;; forms, ensuring each form is compiled before processing the next. This
;; catches macros that expand to require/provide/define etc.

(require (for-syntax racket/base
                     syntax/kerncase)
         (only-in racket/base [#%module-begin base-module-begin])
         camp        ; Re-export camp bindings so they're available in body forms
         punct/doc)  ; Re-export for document struct

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [camp-page-module-begin #%module-begin])
         (all-from-out camp)
         (all-from-out punct/doc)
         camp-page-doc?)

;; Predicate to identify camp/page documents
(define (camp-page-doc? doc)
  (and (document? doc)
       (procedure? (hash-ref (document-metas doc) 'camp-page-body-thunk #f))))

;; ---------------------------------------------------------------------------
;; Form classification helpers (compile-time)

(begin-for-syntax
  (require syntax/kerncase)

  ;; Stop-list for local-expand: kernel forms + require/provide
  (define stop-list
    (append (kernel-form-identifier-list)
            (syntax->list #'(require provide))))

  ;; Check if an identifier matches any in a list
  (define (id-matches? id cmp-ids)
    (and (identifier? id)
         (ormap (λ (cmp) (free-identifier=? id cmp)) cmp-ids)))

  ;; Forms that always lift to module level (including #% variants)
  (define lift-always-ids
    (syntax->list #'(require provide #%require #%provide
                             begin-for-syntax module module*)))

  ;; Definition forms (lift before metadata, body after)
  (define definition-ids
    (syntax->list #'(define-values define-syntaxes)))

  ;; Get the head identifier of a form (if it's an application)
  (define (form-head-id form)
    (syntax-case form ()
      [(head . _) (identifier? #'head) #'head]
      [_ #f])))

;; ---------------------------------------------------------------------------
;; Entry point: #%module-begin

(define-syntax (camp-page-module-begin stx)
  (syntax-case stx ()
    [(_ form ...)
     ;; Get here-path for metadata
     (let ([here-path (let ([src (syntax-source stx)])
                        (cond
                          [(path? src) (path->string src)]
                          [(string? src) src]
                          [else "unknown"]))])
       (with-syntax ([here here-path])
         #'(base-module-begin
            ;; Start recursive processing: not-in-body, empty metadata, empty body
            (camp-page-process here #f () () form ...))))]))

;; ---------------------------------------------------------------------------
;; Recursive form processor
;;
;; This macro processes forms one at a time, emitting lift forms immediately
;; so they're compiled before processing subsequent forms. This ensures macros
;; defined earlier in the file are available when expanding later forms.
;;
;; Arguments:
;;   here-path  - source file path for metadata
;;   in-body?   - #t after seeing metadata or body content
;;   metas      - accumulated ((key val) ...) pairs
;;   bodies     - accumulated body expressions
;;   form ...   - remaining forms to process

(define-syntax (camp-page-process stx)
  (syntax-case stx ()
    ;; Base case: no more forms - emit the final document
    [(_ here-path in-body? ((key val) ...) (body-expr ...))
     #'(begin
         (provide doc)
         (define body-thunk
           (λ () (let () body-expr ...)))
         (define doc
           (document
            (for/fold ([h (hasheq 'here-path here-path
                                  'camp-page-body-thunk body-thunk)])
                      ([k (in-list (list (quote key) ...))]
                       [v (in-list (list val ...))])
              (hash-set h k v))
            '()
            '())))]

    ;; Recursive case: process the next form
    [(_ here-path in-body? metas bodies form0 rest-forms ...)
     (let ([current-form #'form0]
           [datum (syntax-e #'form0)])
       (cond
         ;; Keyword -> metadata, consume next form as value
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

         ;; Everything else: use local-expand to reveal the true form
         [else
          (let* ([expanded (local-expand current-form 'module stop-list)]
                 [head (form-head-id expanded)])
            (cond
              ;; Begin - splice contents and continue
              [(and head (free-identifier=? head #'begin))
               (syntax-case expanded ()
                 [(_ inner ...)
                  #`(camp-page-process here-path in-body? metas bodies
                                       inner ... rest-forms ...)])]

              ;; Forms that always lift - emit immediately and continue
              [(and head (id-matches? head lift-always-ids))
               #`(begin
                   #,expanded
                   (camp-page-process here-path in-body? metas bodies rest-forms ...))]

              ;; Definition forms - lift before metadata, body after
              [(and head (id-matches? head definition-ids))
               (if (syntax-e #'in-body?)
                   ;; After metadata: add to body (append to preserve order)
                   #`(camp-page-process here-path #t metas
                                        (#,@(syntax->list #'bodies) #,expanded) rest-forms ...)
                   ;; Before metadata: emit immediately (lift)
                   #`(begin
                       #,expanded
                       (camp-page-process here-path #f metas bodies rest-forms ...)))]

              ;; Everything else is body content
              [else
               #`(camp-page-process here-path #t metas
                                    (#,@(syntax->list #'bodies) #,expanded) rest-forms ...)]))]))]))

;; ---------------------------------------------------------------------------
;; Reader submodule

(module reader syntax/module-reader
  camp/page)
