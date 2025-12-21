#lang racket/base

(require racket/fasl
         racket/format
         racket/list
         racket/file
         racket/match
         racket/path
         racket/set
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
         copy-static-files
         sync-static-files)

;; ---------------------------------------------------------------------------
;; Collect Pass

(define (collect site)
  (define root-dir (site-root site))
  (define source-ext (site-sources site))
  (define collections (site-collections site))

  (define all-pages
    (for/fold ([pages '()])
              ([coll (in-list collections)])
      (append pages (collect-collection root-dir source-ext coll))))

  (define page-index (build-page-index all-pages))
  (define term-index (build-term-index all-pages))
  (define taxonomy-index (build-taxonomy-index all-pages collections))

  (site-info all-pages term-index page-index taxonomy-index))

;; ---------------------------------------------------------------------------
;; Output Path Helpers

(define (normalize-output-path path-str)
  (define without-leading
    (if (string-prefix? path-str "/")
        (substring path-str 1)
        path-str))
  (define with-index
    (if (or (string=? without-leading "") (string-suffix? without-leading "/"))
        (string-append without-leading "index.html")
        without-leading))
  (string->path with-index))

;; ---------------------------------------------------------------------------
;; Collection Processing

(define (collect-collection site-root source-ext coll)
  (define coll-name (collection-name coll))
  (define source-pattern (collection-source coll))
  (define output-pattern (collection-output-paths coll))

  (define source-dir
    (build-path site-root (source-pattern->directory source-pattern)))

  (define raw-pages
    (for/list ([(path slug doc) (in-sources source-dir source-ext)])
      (list path slug doc)))

  (define sorted-pages (sort-pages raw-pages coll))

  (for/list ([raw-page (in-list sorted-pages)])
    (define source-path (first raw-page))
    (define slug (second raw-page))
    (define doc (third raw-page))
    (define date-val (get-page-date doc))
    (define output-path
      (let ([override (meta-ref doc 'output-path)])
        (if override
            (normalize-output-path override)
            (format-output-path output-pattern slug date-val))))
    (page source-path output-path doc slug coll-name)))

(define (get-page-date doc)
  (define raw-date (meta-ref doc 'date))
  (cond
    [(not raw-date) #f]
    [(date-provider? raw-date) raw-date]
    [(string? raw-date) (iso8601->date raw-date)]
    [else #f]))

;; ---------------------------------------------------------------------------
;; Page Index

(define (build-page-index pages)
  (for/hash ([p (in-list pages)])
    (define slug (page-slug p))
    (define doc (page-doc p))
    (define title (or (meta-ref doc 'title) slug))
    (define url (output-path->url (page-output-path p)))
    (define metas (hash-set (document-metas doc) 'slug slug))
    (values slug (page-link url title metas))))

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
;; Term Index

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
         (when (hash-has-key? idx normalized)
           (log-camp-warning "~a: duplicate term definition: ~a (previously defined elsewhere)"
                             (page-source-path p) term))
         (hash-set idx normalized (string-append page-url "#term-" normalized)))]
      [else index])))

;; ---------------------------------------------------------------------------
;; Taxonomy Index

(define (build-taxonomy-index pages collections)
  (define pages-by-collection
    (for/fold ([grouped (hash)])
              ([p (in-list pages)])
      (define coll-name (page-collection-name p))
      (hash-update grouped coll-name (λ (lst) (cons p lst)) '())))

  (define ordered-pages-by-collection
    (for/hash ([(coll-name pgs) (in-hash pages-by-collection)])
      (values coll-name (reverse pgs))))

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

(define (build-collection-taxonomy-index pages taxonomies)
  (for/hash ([tax-key (in-list taxonomies)])
    (values tax-key (build-single-taxonomy-index pages tax-key))))

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

(define (normalize-taxonomy-value val)
  (cond
    [(not val) '()]
    [(string? val)
     (map string-trim (string-split val ","))]
    [(list? val)
     (map (λ (v) (if (symbol? v) (symbol->string v) (~a v))) val)]
    [else '()]))

(define (page->page-link p)
  (define doc (page-doc p))
  (define slug (page-slug p))
  (define title (or (meta-ref doc 'title) slug))
  (define url (output-path->url (page-output-path p)))
  (define metas (hash-set (document-metas doc) 'slug slug))
  (page-link url title metas))

;; ---------------------------------------------------------------------------
;; Static File Copying

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
;; Static File Sync

(define (enumerate-static-files source-dir)
  (if (not (directory-exists? source-dir))
      '()
      (for/list ([item (in-directory source-dir)]
                 #:when (file-exists? item))
        (path->string (find-relative-path source-dir item)))))

(define (read-manifest manifest-path)
  (if (and manifest-path
           (file-exists? manifest-path)
           (> (file-size manifest-path) 0))
      (call-with-input-file manifest-path fasl->s-exp)
      '()))

(define (write-manifest manifest-path paths)
  (when manifest-path
    (call-with-output-file manifest-path
      (λ (out) (s-exp->fasl paths out))
      #:exists 'replace)))

(define (sync-static-files source-dir dest-dir manifest-path)
  (cond
    [(not (directory-exists? source-dir))
     (log-camp-info "static folder does not exist: ~a" source-dir)
     0]
    [else
     (define current-files (enumerate-static-files source-dir))
     (define old-files (read-manifest manifest-path))

     (define current-set (list->set current-files))
     (define files-to-delete
       (filter (λ (f) (not (set-member? current-set f))) old-files))

     (for ([rel-path (in-list files-to-delete)])
       (define dest-path (build-path dest-dir rel-path))
       (when (file-exists? dest-path)
         (delete-file dest-path)
         (log-camp-debug "deleted orphaned static file: ~a" rel-path)))

     (cleanup-empty-directories dest-dir files-to-delete)

     (make-directory* dest-dir)
     (for ([item (in-directory source-dir)])
       (define relative (find-relative-path source-dir item))
       (define dest-path (build-path dest-dir relative))
       (cond
         [(directory-exists? item)
          (make-directory* dest-path)]
         [(file-exists? item)
          (make-parent-directory* dest-path)
          (copy-file item dest-path #:exists-ok? #t)]))

     (write-manifest manifest-path current-files)

     (length current-files)]))

(define (cleanup-empty-directories base-dir deleted-files)
  (define parent-dirs
    (remove-duplicates
     (filter-map
      (λ (f)
        (define p (path-only f))
        (and p
             (let ([ps (path->string p)])
               (and (not (string=? ps ""))
                    (not (string=? ps "."))
                    ps))))
      deleted-files)))
  (define sorted-dirs
    (sort parent-dirs > #:key (λ (p) (length (explode-path (string->path p))))))
  (for ([dir-str (in-list sorted-dirs)])
    (define full-path (build-path base-dir dir-str))
    (when (and (directory-exists? full-path)
               (null? (directory-list full-path)))
      (delete-directory full-path))))

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

    (copy-static-files static-dir output-dir)
    (when (directory-exists? static-dir)
      (log-camp-debug "copied static files"))

    (define coll-by-name
      (for/hasheq ([c (in-list collections)])
        (values (collection-name c) c)))

    (for ([p (in-list pages)])
      (define coll-name (page-collection-name p))
      (define coll (hash-ref coll-by-name coll-name))
      (define render-fn (resolve-render-function coll site))
      (define body (render-page-body (page-doc p) term-index page-index element-fallback))
      (define ctx (build-context p body coll-name taxonomy-index pages))
      (define html-xexpr (render-fn (page-doc p) ctx))
      (define output-path (build-path output-dir (page-output-path p)))
      (make-parent-directory* output-path)
      (define html-string (xexpr->html5 html-xexpr))
      (call-with-output-file output-path
        (λ (out) (display html-string out))
        #:exists 'replace))

    (log-camp-debug "built ~a pages" (length pages))

    (generate-feeds! site info)))

;; ---------------------------------------------------------------------------
;; Render Function Resolution

(define (resolve-render-function coll site)
  (define render-spec (or (collection-render-with coll)
                          (site-default-render site)))
  (unless render-spec
    (error 'build! "no render function for collection ~a and no site default"
           (collection-name coll)))
  (resolve-module-binding render-spec))

(define (resolve-element-fallback site)
  (define spec (site-element-fallback site))
  (and spec (resolve-module-binding spec)))

(define (resolve-module-binding spec)
  (define mod-path (car spec))
  (define binding (cadr spec))
  (dynamic-require mod-path binding))

;; ---------------------------------------------------------------------------
;; Body Rendering

(define (render-page-body doc term-index page-index element-fallback)
  (define body-thunk (meta-ref doc 'camp-page-body-thunk))
  (cond
    [body-thunk
     (define result (body-thunk))
     (if (and (pair? result) (symbol? (car result)))
         (list result)
         result)]
    [else
     (define result (camp-doc->html-xexpr doc term-index page-index element-fallback))
     (if (>= (length result) 2)
         (cdr result)
         '())]))

;; ---------------------------------------------------------------------------
;; Context Building

(define (build-context p body coll-name taxonomy-index all-pages)
  (define slug (page-slug p))
  (define doc (page-doc p))
  (define coll-pages
    (map page->page-link
         (filter (λ (pg) (equal? (page-collection-name pg) coll-name)) all-pages)))
  (define page-taxonomies (build-page-taxonomies doc taxonomy-index coll-name))
  (define prev-proc (make-nav-proc prev-in slug coll-name coll-pages page-taxonomies taxonomy-index))
  (define next-proc (make-nav-proc next-in slug coll-name coll-pages page-taxonomies taxonomy-index))
  (hasheq 'body body
          'slug slug
          'collection coll-name
          'prev prev-proc
          'next next-proc
          'taxonomies page-taxonomies))

(define (build-page-taxonomies doc taxonomy-index coll-name)
  (define coll-taxes (hash-ref taxonomy-index coll-name #f))
  (if (not coll-taxes)
      (hash)
      (for/hash ([(tax-key _term-hash) (in-hash coll-taxes)])
        (define raw-val (meta-ref doc (string->symbol tax-key)))
        (values tax-key (normalize-taxonomy-value raw-val)))))

(define (make-nav-proc nav-fn slug coll-name coll-pages page-taxonomies taxonomy-index)
  (λ args
    (match args
      ['()
       (nav-fn coll-pages slug)]
      [(list taxonomy-key)
       (define terms (hash-ref page-taxonomies taxonomy-key '()))
       (if (null? terms)
           #f
           (let ([term-pages (get-taxonomy-term-pages taxonomy-index coll-name taxonomy-key (car terms))])
             (nav-fn term-pages slug)))]
      [(list taxonomy-key term)
       (define term-pages (get-taxonomy-term-pages taxonomy-index coll-name taxonomy-key term))
       (nav-fn term-pages slug)])))

(define (get-taxonomy-term-pages taxonomy-index coll-name taxonomy-key term)
  (define coll-taxes (hash-ref taxonomy-index coll-name (hasheq)))
  (define term-hash (hash-ref coll-taxes taxonomy-key (hash)))
  (hash-ref term-hash term '()))
