#lang racket/base

(require racket/format
         racket/list
         racket/path
         racket/string
         punct/doc
         punct/fetch
         gregor
         "structs.rkt"
         "collections.rkt"
         "path-map.rkt"
         "log.rkt"
         "xref.rkt")

(provide collect
         build!)

;; ---------------------------------------------------------------------------
;; Collect Pass
;;
;; Traverses all collections, loads documents, and builds indexes for:
;; - pages: list of page structs
;; - page-index: slug → page-link (for page-ref resolution)
;; - term-index: term-name → url#fragment (for term resolution)
;; - taxonomy-index: collection → taxonomy-key → term → (listof page-link)

(define (collect site)
  (define root-dir (site-root site))
  (define source-ext (site-sources site))
  (define collections (site-collections site))

  ;; Collect all pages from all collections
  (define all-pages
    (for/fold ([pages '()])
              ([coll (in-list collections)])
      (append pages (collect-collection root-dir source-ext coll))))

  ;; Build page index: slug → page-link
  (define page-index (build-page-index all-pages))

  ;; Build term index: term-name → url#term-name
  (define term-index (build-term-index all-pages))

  ;; Build taxonomy index: collection → taxonomy-key → term → (listof page-link)
  (define taxonomy-index (build-taxonomy-index all-pages collections))

  (site-info all-pages term-index page-index taxonomy-index))

;; ---------------------------------------------------------------------------
;; Collection Processing

;; Collect all pages from a single collection
;; Returns: (listof page?)
(define (collect-collection site-root source-ext coll)
  (define coll-name (collection-name coll))
  (define source-pattern (collection-source coll))
  (define output-pattern (collection-output-paths coll))

  ;; Get the source directory relative to site root
  (define source-dir
    (build-path site-root (source-pattern->directory source-pattern)))

  ;; Collect raw page data: (list source-path slug doc)
  (define raw-pages
    (for/list ([(path slug doc) (in-sources source-dir source-ext)])
      (list path slug doc)))

  ;; Sort pages according to collection settings
  (define sorted-pages (sort-pages raw-pages coll))

  ;; Convert to page structs with computed output paths
  (for/list ([raw-page (in-list sorted-pages)])
    (define source-path (first raw-page))
    (define slug (second raw-page))
    (define doc (third raw-page))
    (define date-val (get-page-date doc))
    (define output-path (format-output-path output-pattern slug date-val))
    (page source-path output-path doc slug coll-name)))

;; Extract date from document metadata, parsing if necessary
(define (get-page-date doc)
  (define raw-date (meta-ref doc 'date))
  (cond
    [(not raw-date) #f]
    [(date-provider? raw-date) raw-date]
    [(string? raw-date) (iso8601->date raw-date)]
    [else #f]))

;; ---------------------------------------------------------------------------
;; Page Index

;; Build page index: slug → page-link
;; Uses for/hash (equal?-based) for string keys
(define (build-page-index pages)
  (for/hash ([p (in-list pages)])
    (define slug (page-slug p))
    (define doc (page-doc p))
    (define title (or (meta-ref doc 'title) slug))
    (define url (output-path->url (page-output-path p)))
    (define metas (hash-set (document-metas doc) 'slug slug))
    (values slug (page-link url title metas))))

;; Convert output path to URL (web path with leading /)
(define (output-path->url output-path)
  (define path-str (path->string output-path))
  ;; Convert backslashes to forward slashes (for Windows compatibility)
  (define normalized (string-replace path-str "\\" "/"))
  ;; Remove index.html suffix for clean URLs
  (define clean
    (if (string-suffix? normalized "/index.html")
        (substring normalized 0 (- (string-length normalized) 10))
        normalized))
  ;; Ensure leading slash
  (if (string-prefix? clean "/")
      clean
      (string-append "/" clean)))

;; ---------------------------------------------------------------------------
;; Term Index

;; Build term index: normalized-term-name → url#term-normalized-name
;; Uses hash (equal?-based) for string keys
;; Term names are normalized (lowercased, spaces→hyphens) for consistent lookup.
(define (build-term-index pages)
  (for/fold ([index (hash)])
            ([p (in-list pages)])
    (define doc (page-doc p))
    (define terms-defined (meta-ref doc 'terms-defined))
    (define page-url (output-path->url (page-output-path p)))
    (cond
      [(not terms-defined) index]
      [(list? terms-defined)
       (for/fold ([idx index])
                 ([term (in-list terms-defined)])
         (define normalized (normalize-term-name term))
         ;; Check for duplicate term
         (when (hash-has-key? idx normalized)
           (log-camp-warning "duplicate term definition: ~a (in ~a, previously defined elsewhere)"
                             term (page-slug p)))
         ;; Last definition wins
         (hash-set idx normalized (string-append page-url "#term-" normalized)))]
      [else index])))

;; ---------------------------------------------------------------------------
;; Taxonomy Index

;; Build taxonomy index: collection → taxonomy-key → term → (listof page-link)
(define (build-taxonomy-index pages collections)
  ;; Group pages by collection
  (define pages-by-collection
    (for/fold ([grouped (hasheq)])
              ([p (in-list pages)])
      (define coll-name (page-collection-name p))
      (hash-update grouped coll-name (λ (lst) (cons p lst)) '())))

  ;; Reverse the lists to preserve order (pages were consed in reverse)
  (define ordered-pages-by-collection
    (for/hasheq ([(coll-name pgs) (in-hash pages-by-collection)])
      (values coll-name (reverse pgs))))

  ;; Build index for each collection that has taxonomies
  (for/fold ([index (hasheq)])
            ([coll (in-list collections)])
    (define coll-name (collection-name coll))
    (define taxonomies (collection-taxonomies coll))
    (cond
      [(null? taxonomies) index]
      [else
       (define coll-pages (hash-ref ordered-pages-by-collection coll-name '()))
       (define coll-tax-index
         (build-collection-taxonomy-index coll-pages taxonomies))
       (hash-set index coll-name coll-tax-index)])))

;; Build taxonomy index for a single collection
;; Returns: taxonomy-key → term → (listof page-link)
(define (build-collection-taxonomy-index pages taxonomies)
  (for/hasheq ([tax-key (in-list taxonomies)])
    (values tax-key (build-single-taxonomy-index pages tax-key))))

;; Build index for a single taxonomy within a collection
;; Returns: term → (listof page-link)
;; Uses hash (equal?-based) for string keys
(define (build-single-taxonomy-index pages tax-key)
  (define tax-sym (string->symbol tax-key))
  (for/fold ([index (hash)])
            ([p (in-list pages)])
    (define doc (page-doc p))
    (define raw-val (meta-ref doc tax-sym))
    (define terms (normalize-taxonomy-value raw-val))
    (define pl (page->page-link p))
    (for/fold ([idx index])
              ([term (in-list terms)])
      (hash-update idx term (λ (lst) (append lst (list pl))) '()))))

;; Normalize taxonomy value to a list of strings
;; Handles: comma-separated string, list of symbols, list of strings, #f
(define (normalize-taxonomy-value val)
  (cond
    [(not val) '()]
    [(string? val)
     ;; Split on comma, trim whitespace
     (map string-trim (string-split val ","))]
    [(list? val)
     ;; Convert symbols to strings if needed
     (map (λ (v) (if (symbol? v) (symbol->string v) (~a v))) val)]
    [else '()]))

;; Convert a page struct to a page-link
(define (page->page-link p)
  (define doc (page-doc p))
  (define slug (page-slug p))
  (define title (or (meta-ref doc 'title) slug))
  (define url (output-path->url (page-output-path p)))
  (define metas (hash-set (document-metas doc) 'slug slug))
  (page-link url title metas))

;; ---------------------------------------------------------------------------
;; Build Pass (placeholder)

(define (build! site info)
  ;; TODO: Implement in Phase 4
  (void))
