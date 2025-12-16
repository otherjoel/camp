#lang racket/base

(require racket/contract
         "private/xref.rkt")

(provide/contract
 [defterm (->* () #:rest list? list?)]
 [define-term (->* () #:rest list? list?)]  ; alias for defterm
 [term (->* () #:rest list? list?)]
 [page-ref (->* (string?) #:rest list? list?)])
