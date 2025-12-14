#lang racket/base

(require racket/string
         punct/core)

(provide defterm
         term
         page-ref)

(define (defterm name . content)
  ;; Add term to the document's terms-defined metadata list
  (cons-to-metas-list 'terms-defined name)
  `(term-definition ((name ,name)) ,@content))

(define (term name)
  `(term ((name ,name))))

(define (page-ref slug-or-text . content)
  (define slug (string-replace slug-or-text " " "-"))
  (if (null? content)
      `(page-ref ((slug ,slug)))
      `(page-ref ((slug ,slug)) ,@content)))
