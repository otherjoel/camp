#lang racket/base

(require hash-view)

(provide
 (hash-view-out site)
 (hash-view-out collection)
 (hash-view-out feed-config)
 (struct-out page)
 (struct-out page-link)
 (struct-out site-info)
 (hash-view-out context))

;; ---------------------------------------------------------------------------
;; Configuration hash-views

(hash-view site
  (title
   url
   founded
   authors
   sources
   static-folder
   output-folder
   collections
   [root #:default #f]           ; site root directory, set by load-site
   [deploy-script #:default #f]
   [default-render #:default #f]
   [element-fallback #:default #f]
   [feeds #:default '()]))

(hash-view collection
  (name
   source
   output-paths
   [render-with #:default #f]
   [order #:default "descending"]
   [sort-key #:default "date"]
   [taxonomies #:default '()]))

(hash-view feed-config
  (filename
   collections
   render-with))

;; ---------------------------------------------------------------------------
;; Structs

(struct page (source-path output-path doc slug collection-name) #:transparent)
(struct page-link (url title metas) #:transparent)
(struct site-info (pages term-index page-index taxonomy-index
                   pages-by-collection       ; collection-name → (listof page?)
                   page-links-by-collection  ; collection-name → (listof page-link?)
                   page-by-slug)             ; slug → page?
  #:transparent)

;; ---------------------------------------------------------------------------
;; Render context

(hash-view context
  (body
   slug
   collection
   prev   ; procedure: () -> page-link?, (taxonomy) -> page-link?, (taxonomy term) -> page-link?
   next   ; procedure: same calling conventions as prev
   taxonomies))
