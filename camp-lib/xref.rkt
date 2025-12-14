#lang racket/base

(require racket/contract
         "private/xref.rkt")

(provide/contract
 [defterm (->* (string?) #:rest list? list?)]
 [term (-> string? list?)]
 [page-ref (->* (string?) #:rest list? list?)])
