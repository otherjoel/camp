#lang racket/base

(require racket/path
         racket/list
         racket/format
         racket/string
         racket/rerequire
         syntax/modresolve
         pkg/path
         setup/getinfo
         punct/doc
         punct/fetch
         "structs.rkt"
         "filter.rkt"
         "xref.rkt")

(provide file-path->site-path
         load-site
         filter-pages
         current-site-info
         get-collection
         get-taxonomy-terms
         get-taxonomy-pages
         prev-in
         next-in)

;; ---------------------------------------------------------------------------
;; Site-Info Parameter

(define current-site-info (make-parameter #f))

;; ---------------------------------------------------------------------------
;; Package Info Helpers

(define (file-path->site-path file-path)
  (define abs-path (simplify-path (path->complete-path file-path)))
  (define-values (pkg-name subpath) (path->pkg+subpath abs-path))
  (unless pkg-name
    (error 'file-path->site-path
           "file is not in a package: ~a" abs-path))
  ;; Compute package root by stripping subpath from abs-path
  (define pkg-root
    (cond
      [(eq? subpath 'same) abs-path]
      [(path? subpath)
       (define subpath-parts (explode-path subpath))
       (for/fold ([p abs-path]) ([_ (in-list subpath-parts)])
         (simplify-path (build-path p 'up)))]
      [else (error 'file-path->site-path
                   "unexpected subpath value: ~a" subpath)]))
  ;; Get info procedure for this package
  (define get-pkg-info (get-info/full pkg-root))
  (unless get-pkg-info
    (error 'file-path->site-path
           "no info.rkt found in package root: ~a" pkg-root))
  ;; Look up camp-site field
  (define site-mod-path (get-pkg-info 'camp-site (λ () #f)))
  (unless site-mod-path
    (error 'file-path->site-path
           "info.rkt does not define 'camp-site: ~a" pkg-root))
  ;; Resolve the module path relative to the package root
  (simplify-path (build-path pkg-root site-mod-path)))

;; ---------------------------------------------------------------------------
;; Site Loading

(define (load-site mod-path)
  (define resolved
    (cond
      ;; Hash with 'path key (e.g., a book config) - discover site from package
      [(and (hash? mod-path) (hash-has-key? mod-path 'path))
       (file-path->site-path (hash-ref mod-path 'path))]
      [(and (path-string? mod-path) (absolute-path? mod-path))
       (if (path? mod-path) mod-path (string->path mod-path))]
      [(path-string? mod-path)
       (simplify-path (path->complete-path (if (path? mod-path)
                                               mod-path
                                               (string->path mod-path))))]
      [else
       (resolve-module-path mod-path #f)]))
  (dynamic-rerequire resolved)
  (define site-config (dynamic-require resolved 'toml))
  (define site-root (simplify-path (build-path resolved 'up)))
  (hash-set site-config 'root site-root))

;; ---------------------------------------------------------------------------
;; Collection Retrieval

(define (get-collection name
                        #:limit [limit #f]
                        #:full-docs? [full-docs? #f])
  (define info (current-site-info))
  (unless info
    (error 'get-collection "no site-info available (not in build context)"))
  (define result
    (if full-docs?
        (hash-ref (site-info-pages-by-collection info) name #f)
        (hash-ref (site-info-page-links-by-collection info) name #f)))
  (unless result
    (error 'get-collection "collection not found: ~a" name))
  (define limited
    (if (and limit (> (length result) limit))
        (take result limit)
        result))
  (if full-docs?
      (map page-doc limited)
      limited))

(define (page->page-link p)
  (define doc (page-doc p))
  (define slug (page-slug p))
  (define title (or (meta-ref doc 'title) slug))
  (define url (output-path->url (page-output-path p)))
  (define metas (hash-set (document-metas doc) 'slug slug))
  (page-link url title metas))

(define (output-path->url output-path)
  (define path-str (path->string output-path))
  (define normalized (string-replace path-str "\\" "/"))
  (define clean
    (if (string-suffix? normalized "/index.html")
        (substring normalized 0 (- (string-length normalized) 10))
        normalized))
  (if (string-prefix? clean "/")
      clean
      (string-append "/" clean)))

;; ---------------------------------------------------------------------------
;; Taxonomy Functions

(define (get-taxonomy-terms collection-name taxonomy-key)
  (define info (current-site-info))
  (unless info
    (error 'get-taxonomy-terms "no site-info available (not in build context)"))
  (define pages (site-info-pages info))
  (define tax-sym (string->symbol taxonomy-key))
  (define coll-pages
    (filter (λ (p) (equal? (page-collection-name p) collection-name)) pages))
  (define seen (make-hash))
  (define terms
    (for*/list ([p (in-list coll-pages)]
                [term (in-list (normalize-taxonomy-value
                                (meta-ref (page-doc p) tax-sym)))]
                #:unless (hash-has-key? seen term))
      (hash-set! seen term #t)
      term))
  terms)

(define (get-taxonomy-pages collection-name taxonomy-key [term #f])
  (define info (current-site-info))
  (unless info
    (error 'get-taxonomy-pages "no site-info available (not in build context)"))
  (define tax-index (site-info-taxonomy-index info))
  (define coll-taxes (hash-ref tax-index collection-name #f))
  (unless coll-taxes
    (error 'get-taxonomy-pages "collection not found or has no taxonomies: ~a" collection-name))
  (define term-hash (hash-ref coll-taxes taxonomy-key #f))
  (unless term-hash
    (error 'get-taxonomy-pages "taxonomy not found: ~a in collection ~a" taxonomy-key collection-name))
  (if term
      (hash-ref term-hash term '())
      term-hash))


;; ---------------------------------------------------------------------------
;; Page Navigation

(define (page-link-slug pl)
  (hash-ref (page-link-metas pl) 'slug))

(define (prev-in pages current-slug)
  (define normalized-current (normalize-slug current-slug))
  (let loop ([pages pages] [prev #f])
    (cond
      [(null? pages) #f]
      [(equal? (normalize-slug (page-link-slug (car pages))) normalized-current) prev]
      [else (loop (cdr pages) (car pages))])))

(define (next-in pages current-slug)
  (define normalized-current (normalize-slug current-slug))
  (let loop ([pages pages])
    (cond
      [(null? pages) #f]
      [(null? (cdr pages)) #f]
      [(equal? (normalize-slug (page-link-slug (car pages))) normalized-current) (cadr pages)]
      [else (loop (cdr pages))])))
