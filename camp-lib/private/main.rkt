#lang racket/base

(require racket/path
         racket/list
         racket/format
         racket/string
         racket/rerequire
         syntax/modresolve
         punct/doc
         punct/fetch
         "structs.rkt")

(provide load-site
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
;; Site Loading

(define (load-site mod-path)
  (define resolved
    (cond
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
  (define pages (site-info-pages info))
  (define coll-pages
    (filter (λ (p) (equal? (page-collection-name p) name)) pages))
  (when (null? coll-pages)
    (error 'get-collection "collection not found: ~a" name))
  (define limited-pages
    (if (and limit (> (length coll-pages) limit))
        (take coll-pages limit)
        coll-pages))
  (if full-docs?
      (map page-doc limited-pages)
      (map page->page-link limited-pages)))

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

(define (normalize-taxonomy-value val)
  (cond
    [(not val) '()]
    [(string? val)
     (map string-trim (string-split val ","))]
    [(list? val)
     (map (λ (v) (if (symbol? v) (symbol->string v) (~a v))) val)]
    [else '()]))

;; ---------------------------------------------------------------------------
;; Page Navigation

(define (page-link-slug pl)
  (hash-ref (page-link-metas pl) 'slug))

(define (prev-in pages current-slug)
  (let loop ([pages pages] [prev #f])
    (cond
      [(null? pages) #f]
      [(equal? (page-link-slug (car pages)) current-slug) prev]
      [else (loop (cdr pages) (car pages))])))

(define (next-in pages current-slug)
  (let loop ([pages pages])
    (cond
      [(null? pages) #f]
      [(null? (cdr pages)) #f]
      [(equal? (page-link-slug (car pages)) current-slug) (cadr pages)]
      [else (loop (cdr pages))])))
