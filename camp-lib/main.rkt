#lang racket/base

(require racket/contract
         hash-view
         gregor
         "private/structs.rkt"
         "private/main.rkt")

(provide
 (hash-view-out site)
 (hash-view-out collection)
 (hash-view-out feed-config)
 (hash-view-out context)
 (struct-out page)
 (struct-out page-link)
 (struct-out site-info)
 current-site-info)

(define (~d pattern v)
  (~t (if (string? v) (iso8601->date v) v) pattern))

(provide/contract
 [~d (-> string? (or/c string? date-provider?) string?)]
 [load-site (-> (or/c path-string? module-path?) site?)]
 [get-collection (->* (string?)
                      (#:limit (or/c #f exact-positive-integer?)
                       #:full-docs? boolean?)
                      list?)]
 [get-taxonomy-terms (-> string? string? (listof string?))]
 [get-taxonomy-pages (->* (string? string?)
                          ((or/c string? #f))
                          (or/c list? hash?))]
 [prev-in (-> (listof page-link?) string? (or/c page-link? #f))]
 [next-in (-> (listof page-link?) string? (or/c page-link? #f))])
