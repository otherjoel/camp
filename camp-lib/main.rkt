#lang racket/base

(require racket/contract
         hash-view
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

(provide/contract
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
