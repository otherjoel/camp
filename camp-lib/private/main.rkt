#lang racket/base

(require racket/path
         syntax/modresolve
         "structs.rkt")

(provide load-site
         get-collection
         get-taxonomy-terms
         get-taxonomy-pages
         prev-in
         next-in)

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
  (define site-config (dynamic-require resolved 'toml))
  ;; Add site root directory (parent of the config file)
  (define site-root (simplify-path (build-path resolved 'up)))
  (hash-set site-config 'root site-root))

;; ---------------------------------------------------------------------------
;; Collection Retrieval

(define (get-collection name
                        #:limit [limit #f]
                        #:full-docs? [full-docs? #f])
  ;; TODO: Implement - requires access to current site-info
  '())

;; ---------------------------------------------------------------------------
;; Taxonomy Functions

(define (get-taxonomy-terms collection-name taxonomy-key)
  ;; TODO: Implement
  '())

(define (get-taxonomy-pages collection-name taxonomy-key [term #f])
  ;; TODO: Implement
  (if term '() (hasheq)))

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
