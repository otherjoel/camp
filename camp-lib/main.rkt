#lang racket/base

(require racket/contract
         hash-view
         gregor
         punct/doc
         "private/structs.rkt"
         "private/main.rkt"
         "private/book.rkt"
         "private/typst-render.rkt"
         "private/html-render.rkt"
         "private/book-build.rkt")

(provide
 (hash-view-out site)
 (hash-view-out collection)
 (hash-view-out feed-config)
 (hash-view-out context)
 (hash-view-out book)
 (hash-view-out book-part)
 (hash-view-out part)
 (hash-view-out chapter)
 (struct-out page)
 (struct-out page-link)
 (struct-out site-info)
 (struct-out document)
 current-site-info
 file-path->site-path
 ;; HTML rendering
 camp-doc->html-xexpr
 ;; Typst rendering
 camp-typst-render%
 camp-doc->typst
 escape-typst-text
 escape-typst-string
 default-typst-tag)

(define (~d pattern v)
  (~t (if (string? v) (iso8601->date v) v) pattern))

(provide/contract
 [~d (-> string? (or/c string? date-provider?) string?)]
 [load-site (-> (or/c path-string? module-path? (and/c hash? (λ (h) (hash-has-key? h 'path)))) site?)]
 [load-book (-> path-string? book?)]
 [get-collection (->* (string?)
                      (#:limit (or/c #f exact-positive-integer?)
                       #:full-docs? boolean?)
                      list?)]
 [get-taxonomy-terms (-> string? string? (listof string?))]
 [get-taxonomy-pages (->* (string? string?)
                          ((or/c string? #f))
                          (or/c list? hash?))]
 [prev-in (-> (listof page-link?) string? (or/c page-link? #f))]
 [next-in (-> (listof page-link?) string? (or/c page-link? #f))]
 [filter-pages (->* ((listof page-link?))
                    (#:date-from (or/c date-provider? #f)
                     #:date-to (or/c date-provider? #f)
                     #:date-key symbol?
                     #:taxonomies hash?)
                    (listof page-link?))]
 ;; Book building
 [build-book! (-> book? (or/c path? #f))]
 [gather-book-parts (-> book? site? site-info? list?)])
