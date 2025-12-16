#lang racket/base

;; camp/page - A lang for structural/organizational pages
;;
;; Unlike #lang punct, this lang doesn't process content through CommonMark.
;; Instead, body expressions are wrapped in a thunk and evaluated at render
;; time when site-info is available.
;;
;; Uses free-identifier=? for robust handling of require/provide/define forms,
;; including renamed imports and macros.

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
;; Form classification (compile-time)

(begin-for-syntax
  ;; Check if an identifier matches any in a list
  (define (id-matches? id cmp-ids)
    (and (identifier? id)
         (ormap (λ (cmp) (free-identifier=? id cmp)) cmp-ids)))

  ;; Forms that always lift to module level
  (define lift-always-ids
    (syntax->list #'(require provide begin-for-syntax module module*)))

  ;; Definition forms (lift before metadata, body after)
  (define definition-ids
    (syntax->list #'(define define-values define-syntax define-syntaxes))))

;; ---------------------------------------------------------------------------
;; Custom #%module-begin

(define-syntax (camp-page-module-begin stx)
  (syntax-case stx ()
    [(_ form ...)
     (let ()
       ;; Process all forms, categorizing them
       (define-values (lifted-forms metadata-pairs body-forms)
         (process-forms (syntax->list #'(form ...)) #f '() '() '()))

       ;; Build the module body
       (with-syntax ([(lifted ...) (reverse lifted-forms)]
                     [(body ...) (reverse body-forms)]
                     [meta-hash (build-meta-hash metadata-pairs stx)])
         #'(base-module-begin
            ;; Lifted forms (require, provide, define before metadata)
            lifted ...

            ;; Export doc (camp and punct/doc are already provided by the language)
            (provide doc)

            ;; The body thunk - evaluated at render time
            (define body-thunk
              (λ () (let () body ...)))

            ;; Create Punct-compatible document with thunk in metadata
            (define doc
              (document
               (hash-set meta-hash 'camp-page-body-thunk body-thunk)
               '()
               '())))))]))

;; ---------------------------------------------------------------------------
;; Form Processing (compile-time)

(begin-for-syntax
  ;; Process forms, returning (values lifted metadata body)
  ;; in-body?: once we've seen metadata or body content, definitions go to body
  (define (process-forms forms in-body? lifted metadata body)
    (if (null? forms)
        (values lifted metadata body)
        (let ([form (car forms)]
              [rest (cdr forms)])
          (process-one-form form rest in-body? lifted metadata body))))

  ;; Get the head identifier of a form (if it's an application)
  (define (form-head-id form)
    (syntax-case form ()
      [(head . _) (identifier? #'head) #'head]
      [_ #f]))

  ;; Process a single form by examining its head identifier
  (define (process-one-form form rest in-body? lifted metadata body)
    (let ([datum (syntax-e form)])
      (cond
        ;; Keyword -> metadata, consume next form as value
        [(keyword? datum)
         (when (null? rest)
           (raise-syntax-error 'camp/page
                               (format "metadata keyword ~a has no value" datum)
                               form))
         (let ([key (string->symbol (keyword->string datum))]
               [val (car rest)])
           (process-forms (cdr rest) #t lifted
                          (cons (cons key val) metadata) body))]

        ;; Check the head identifier of the form
        [else
         (let ([head (form-head-id form)])
           (cond
             ;; Begin - splice and recurse
             [(and head (free-identifier=? head #'begin))
              (syntax-case form ()
                [(_ inner ...)
                 (process-forms (append (syntax->list #'(inner ...)) rest)
                                in-body? lifted metadata body)])]

             ;; Forms that always lift (require, provide, begin-for-syntax, module, module*)
             [(and head (id-matches? head lift-always-ids))
              (process-forms rest in-body? (cons form lifted) metadata body)]

             ;; Definition forms - lift before metadata, body after
             [(and head (id-matches? head definition-ids))
              (if in-body?
                  (process-forms rest #t lifted metadata (cons form body))
                  (process-forms rest #f (cons form lifted) metadata body))]

             ;; Everything else is body content
             [else
              (process-forms rest #t lifted metadata (cons form body))]))])))

  ;; Build metadata hash expression from alist
  (define (build-meta-hash pairs stx)
    (define here-path
      (let ([src (syntax-source stx)])
        (cond
          [(path? src) (path->string src)]
          [(string? src) src]
          [else "unknown"])))
    (for/fold ([h #`(hasheq 'here-path #,here-path)])
              ([pair (in-list (reverse pairs))])
      (let ([key (car pair)]
            [val (cdr pair)])
        #`(hash-set #,h '#,key #,val)))))

;; ---------------------------------------------------------------------------
;; Reader submodule

(module reader syntax/module-reader
  camp/page)
