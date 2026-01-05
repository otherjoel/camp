#lang racket/base

(module reader toml/config/custom
  (require splitflap/constructs
           gregor
           (only-in camp/private/path-map
                    source-path-pattern?
                    output-path-pattern?
                    non-rkt-file-extension?))

  (define (author-string? s) ; "Name (email)" format
    (and (string? s)
         (regexp-match? #px".+\\s+\\(.+\\)" s)))

  (define (list-of-authors? v)
    (and (list? v) (andmap author-string? v)))

  #:schema
  ([title non-empty-string? required]
   [url valid-url-string? required]
   [founded date-provider? required]
   [authors list-of-authors? required]
   [sources non-rkt-file-extension? (optional ".md.rkt")]
   [static-folder path-string? (optional "static")]
   [output-folder path-string? (optional "publish")]
   [deploy-script path-string? (optional #f)]
   [default-render readable-datum? (optional #f)]
   [collections
    (array-of table
              [name non-empty-string? required]
              [source source-path-pattern? required]
              [output-paths output-path-pattern? required]
              [render-with readable-datum? (optional #f)]
              [order (or/c "ascending" "descending") (optional "descending")]
              [sort-key non-empty-string? (optional "date")]
              [taxonomies (listof non-empty-string?) (optional '())])
    required]
   [feeds
    (array-of table
              [filename path-string? required]
              [collections (listof non-empty-string?) required]
              [render-with readable-datum? required])
    optional]))
