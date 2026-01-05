#lang racket/base

(module reader toml/config/custom
  (require gregor)

  (define (list-of-non-empty-strings? v)
    (and (list? v) (andmap non-empty-string? v)))

  ;; Taxonomy filter: hash where values are strings or arrays of strings
  (define (taxonomy-filter? v)
    (and (hash? v)
         (for/and ([(k val) (in-hash v)])
           (or (string? val)
               (and (list? val) (andmap string? val))))))

  #:schema
  ([render-with readable-datum? required]
   [output-folder non-empty-string? required]
   [includes list-of-non-empty-strings? (optional '())]

   [parts
    (array-of table
              [name non-empty-string? required]
              [pages list-of-non-empty-strings? (optional '())]
              [collections list-of-non-empty-strings? (optional '())]
              [date-from date-provider? (optional #f)]
              [date-to date-provider? (optional #f)]
              [taxonomies taxonomy-filter? (optional #hasheq())])
    required]))
