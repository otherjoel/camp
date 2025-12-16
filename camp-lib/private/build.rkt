#lang racket/base

(require racket/format
         racket/list
         racket/file
         racket/match
         racket/path
         racket/string
         punct/doc
         punct/fetch
         gregor
         html-printer
         "structs.rkt"
         "collections.rkt"
         "path-map.rkt"
         "log.rkt"
         "xref.rkt"
         "html-render.rkt"
         "main.rkt"
         "feeds.rkt")

(provide collect
         build!
         copy-static-files)

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
;; Output Path Helpers

;; Normalize a user-provided output-path meta value.
;; Handles trailing "/" → "index.html" conversion and strips leading slash.
(define (normalize-output-path path-str)
  ;; Strip leading slash if present (output paths should be relative)
  (define without-leading
    (if (string-prefix? path-str "/")
        (substring path-str 1)
        path-str))
  ;; Handle trailing slash → index.html
  (define with-index
    (if (or (string=? without-leading "") (string-suffix? without-leading "/"))
        (string-append without-leading "index.html")
        without-leading))
  (string->path with-index))

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
    ;; Allow per-page output-path override via metadata
    (define output-path
      (let ([override (meta-ref doc 'output-path)])
        (if override
            (normalize-output-path override)
            (format-output-path output-pattern slug date-val))))
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
;; Uses hash (equal?-based) for string keys at all levels.
(define (build-taxonomy-index pages collections)
  ;; Group pages by collection
  (define pages-by-collection
    (for/fold ([grouped (hash)])
              ([p (in-list pages)])
      (define coll-name (page-collection-name p))
      (hash-update grouped coll-name (λ (lst) (cons p lst)) '())))

  ;; Reverse the lists to preserve order (pages were consed in reverse)
  (define ordered-pages-by-collection
    (for/hash ([(coll-name pgs) (in-hash pages-by-collection)])
      (values coll-name (reverse pgs))))

  ;; Build index for each collection that has taxonomies
  (for/fold ([index (hash)])
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
;; Uses hash (equal?-based) for string keys
(define (build-collection-taxonomy-index pages taxonomies)
  (for/hash ([tax-key (in-list taxonomies)])
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
;; Static File Copying

;; Copy static folder contents to output folder, preserving directory structure.
;; If source doesn't exist, does nothing (graceful handling).
(define (copy-static-files source-dir dest-dir)
  (when (directory-exists? source-dir)
    (make-directory* dest-dir)
    (for ([item (in-directory source-dir)])
      (define relative (find-relative-path source-dir item))
      (define dest-path (build-path dest-dir relative))
      (cond
        [(directory-exists? item)
         (make-directory* dest-path)]
        [(file-exists? item)
         (make-parent-directory* dest-path)
         (copy-file item dest-path #:exists-ok? #t)]))))

;; ---------------------------------------------------------------------------
;; Build Pass

(define (build! site info)
  (parameterize ([current-site-info info])
    (define root-dir (site-root site))
    (define output-dir (build-path root-dir (site-output-folder site)))
    (define static-dir (build-path root-dir (site-static-folder site)))
    (define collections (site-collections site))
    (define element-fallback (resolve-element-fallback site))
    (define term-index (site-info-term-index info))
    (define page-index (site-info-page-index info))
    (define taxonomy-index (site-info-taxonomy-index info))
    (define pages (site-info-pages info))

    ;; Copy static files first
    (copy-static-files static-dir output-dir)
    (when (directory-exists? static-dir)
      (log-camp-info "copied static files"))

    ;; Build a lookup from collection name to collection config
    (define coll-by-name
      (for/hasheq ([c (in-list collections)])
        (values (collection-name c) c)))

    ;; Render each page
    (for ([p (in-list pages)])
      (define coll-name (page-collection-name p))
      (define coll (hash-ref coll-by-name coll-name))
      (define render-fn (resolve-render-function coll site))

      ;; Render document body
      ;; For camp/page documents, call the deferred body thunk
      ;; For punct documents, render with xref resolution
      (define body (render-page-body (page-doc p) term-index page-index element-fallback))

      ;; Build context for render function
      (define ctx (build-context p body coll-name taxonomy-index pages))

      ;; Call render function
      (define html-xexpr (render-fn (page-doc p) ctx))

      ;; Write to output file
      (define output-path (build-path output-dir (page-output-path p)))
      (make-parent-directory* output-path)
      (define html-string (xexpr->html5 html-xexpr))
      (call-with-output-file output-path
        (λ (out) (display html-string out))
        #:exists 'replace))

    (log-camp-info "built ~a pages" (length pages))

    ;; Generate feeds
    (generate-feeds! site info)))

;; ---------------------------------------------------------------------------
;; Render Function Resolution

;; Resolve a collection's render function, falling back to site default
(define (resolve-render-function coll site)
  (define render-spec (or (collection-render-with coll)
                          (site-default-render site)))
  (unless render-spec
    (error 'build! "no render function for collection ~a and no site default"
           (collection-name coll)))
  (resolve-module-binding render-spec))

;; Resolve site's element-fallback function (or #f if none)
(define (resolve-element-fallback site)
  (define spec (site-element-fallback site))
  (and spec (resolve-module-binding spec)))

;; Resolve a '(module-path binding-name) spec to a function
(define (resolve-module-binding spec)
  (define mod-path (car spec))
  (define binding (cadr spec))
  (dynamic-require mod-path binding))

;; ---------------------------------------------------------------------------
;; Body Rendering

;; Render document body, handling both punct and camp/page documents
;; For camp/page: calls the deferred body thunk (site-info is already parameterized)
;; For punct: renders using camp-html-render% with xref resolution
(define (render-page-body doc term-index page-index element-fallback)
  (define body-thunk (meta-ref doc 'camp-page-body-thunk))
  (cond
    ;; camp/page document: call the thunk to get body content
    [body-thunk
     (define result (body-thunk))
     ;; Normalize result to list of xexprs
     ;; Thunk may return a single xexpr or a list
     (if (and (pair? result) (symbol? (car result)))
         (list result)  ; single xexpr, wrap in list
         result)]       ; already a list
    ;; punct document: render with xref resolution
    [else
     (define result (camp-doc->html-xexpr doc term-index page-index element-fallback))
     ;; camp-doc->html-xexpr returns (article ...body...), extract the body
     ;; Handle empty documents gracefully
     (if (>= (length result) 2)
         (cdr result)
         '())]))

;; ---------------------------------------------------------------------------
;; Context Building

;; Build the context hash-view for a page
(define (build-context p body coll-name taxonomy-index all-pages)
  (define slug (page-slug p))
  (define doc (page-doc p))

  ;; Get pages in this collection (in order)
  (define coll-pages
    (map page->page-link
         (filter (λ (pg) (equal? (page-collection-name pg) coll-name)) all-pages)))

  ;; Build normalized taxonomies hash for this page
  (define page-taxonomies (build-page-taxonomies doc taxonomy-index coll-name))

  ;; Build prev/next procedures
  (define prev-proc (make-nav-proc prev-in slug coll-name coll-pages page-taxonomies taxonomy-index))
  (define next-proc (make-nav-proc next-in slug coll-name coll-pages page-taxonomies taxonomy-index))

  ;; Return context as a hash (works with hash-view accessors)
  (hasheq 'body body
          'slug slug
          'collection coll-name
          'prev prev-proc
          'next next-proc
          'taxonomies page-taxonomies))

;; Build hash of taxonomy-key → (listof string) for a page
(define (build-page-taxonomies doc taxonomy-index coll-name)
  (define coll-taxes (hash-ref taxonomy-index coll-name #f))
  (if (not coll-taxes)
      (hasheq)
      (for/hasheq ([(tax-key _term-hash) (in-hash coll-taxes)])
        (define raw-val (meta-ref doc (string->symbol tax-key)))
        (values tax-key (normalize-taxonomy-value raw-val)))))

;; Create a prev or next navigation procedure with 0-2 arg calling convention
(define (make-nav-proc nav-fn slug coll-name coll-pages page-taxonomies taxonomy-index)
  (λ args
    (match args
      ;; 0 args: navigate within collection
      ['()
       (nav-fn coll-pages slug)]
      ;; 1 arg: navigate within first value of given taxonomy
      [(list taxonomy-key)
       (define terms (hash-ref page-taxonomies taxonomy-key '()))
       (if (null? terms)
           #f
           (let ([term-pages (get-taxonomy-term-pages taxonomy-index coll-name taxonomy-key (car terms))])
             (nav-fn term-pages slug)))]
      ;; 2 args: navigate within specific taxonomy term
      [(list taxonomy-key term)
       (define term-pages (get-taxonomy-term-pages taxonomy-index coll-name taxonomy-key term))
       (nav-fn term-pages slug)])))

;; Get page-links for a specific taxonomy term (from pre-built index)
(define (get-taxonomy-term-pages taxonomy-index coll-name taxonomy-key term)
  (define coll-taxes (hash-ref taxonomy-index coll-name (hasheq)))
  (define term-hash (hash-ref coll-taxes taxonomy-key (hash)))
  (hash-ref term-hash term '()))
