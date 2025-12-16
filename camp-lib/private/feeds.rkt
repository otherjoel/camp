#lang racket/base

;; Feed generation (Atom/RSS)
;;
;; Generates Atom and RSS feeds using the Splitflap library.

(require racket/list
         racket/match
         racket/path
         racket/string
         racket/file
         gregor
         splitflap
         punct/doc
         punct/fetch
         "structs.rkt"
         "main.rkt"
         "log.rkt")

(provide parse-author
         make-feed-tag-uri
         filter-feed-pages
         page-link->feed-item
         generate-feed
         generate-feeds!)

;; ---------------------------------------------------------------------------
;; Author Parsing

;; Parse author string in format "Name (email)" to a splitflap person struct.
;; Example: "Marian Paroo (marian@example.com)" → (person "Marian Paroo" "marian@example.com")
(define (parse-author author-str)
  (match (regexp-match #px"^(.+)\\s+\\(([^)]+)\\)$" author-str)
    [(list _ name email)
     (person (string-trim name) (string-trim email))]
    [_ (error 'parse-author "invalid author format: ~a (expected \"Name (email)\")" author-str)]))

;; ---------------------------------------------------------------------------
;; Tag URI Generation

;; Create a tag URI for a feed.
;; Uses the domain from site URL, the founded date year, and collection names as specific.
(define (make-feed-tag-uri site-url founded-date collection-names)
  (define domain (url-domain site-url))
  (define year (number->string (->year founded-date)))
  ;; Join collection names with comma, normalize for tag URI safety
  (define specific (normalize-tag-specific (string-join collection-names ",")))
  (mint-tag-uri domain year specific))

;; ---------------------------------------------------------------------------
;; Page Filtering

;; Filter pages for feed inclusion.
;; Excludes: pages with draft? = true, pages without a date
(define (filter-feed-pages page-links)
  (filter (lambda (pl)
            (define metas (page-link-metas pl))
            (define has-date? (hash-ref metas 'date #f))
            (define is-draft? (hash-ref metas 'draft? #f))
            (and has-date? (not is-draft?)))
          page-links))

;; ---------------------------------------------------------------------------
;; Feed Item Conversion

;; Convert a page-link to a splitflap feed-item.
;; Parameters:
;;   pl - page-link struct
;;   feed-tag-uri - the feed's tag URI (used to derive item IDs)
;;   site-url - base site URL for absolute URLs
;;   author - splitflap person struct
;;   render-fn - function (doc → xexpr) for entry content
(define (page-link->feed-item pl feed-tag-uri site-url author render-fn)
  (define metas (page-link-metas pl))
  (define slug (hash-ref metas 'slug))
  (define title (page-link-title pl))
  (define relative-url (page-link-url pl))
  (define date-val (hash-ref metas 'date))

  ;; Create absolute URL
  ;; url-join expects a relative path without leading slash
  (define relative-for-join
    (if (string-prefix? relative-url "/")
        (substring relative-url 1)
        relative-url))
  (define absolute-url (url-join site-url relative-for-join))

  ;; Create item-specific tag URI
  (define item-tag-uri (append-specific feed-tag-uri (normalize-tag-specific slug)))

  ;; Parse date to moment
  (define pub-moment (date-string->moment date-val))

  ;; Load the full document to render content
  ;; Note: For feeds, we need to load the doc from site-info
  ;; The render function takes a doc and returns xexpr content
  (define content (get-feed-content slug render-fn))

  (feed-item item-tag-uri
             absolute-url
             title
             author
             pub-moment
             pub-moment  ; updated = published (no separate tracking)
             content))

;; Convert date value (string or date) to moment for splitflap
(define (date-string->moment date-val)
  (cond
    [(moment? date-val) date-val]
    [(date? date-val) (infer-moment (date->iso8601 date-val))]
    [(string? date-val) (infer-moment date-val)]
    [else (infer-moment)]))

;; Get feed content for a page by looking up the doc from site-info
(define (get-feed-content slug render-fn)
  (define info (current-site-info))
  (unless info
    (error 'get-feed-content "no site-info available"))
  (define pages (site-info-pages info))
  (define matching-page
    (findf (lambda (p) (equal? (page-slug p) slug)) pages))
  (if matching-page
      (render-fn (page-doc matching-page))
      '(p "Content unavailable")))

;; ---------------------------------------------------------------------------
;; Feed Generation

;; Generate a feed for a single feed configuration.
;; Returns the XML string for the feed.
(define (generate-feed site info feed-cfg)
  (define the-site-url (site-url site))
  (define the-site-title (site-title site))
  (define founded (site-founded site))
  (define authors (site-authors site))
  (define root-dir (site-root site))
  (define output-dir (build-path root-dir (site-output-folder site)))

  ;; Feed config values
  (define filename (feed-config-filename feed-cfg))
  (define coll-names (feed-config-collections feed-cfg))
  (define render-spec (feed-config-render-with feed-cfg))

  ;; Resolve render function
  (define render-fn (resolve-module-binding render-spec))

  ;; Determine feed format from extension
  (define feed-format
    (cond
      [(path-has-extension? (string->path filename) #".atom") 'atom]
      [(path-has-extension? (string->path filename) #".rss") 'rss]
      [else (error 'generate-feed "unknown feed extension: ~a (expected .atom or .rss)" filename)]))

  ;; Create feed tag URI
  (define feed-tag-uri (make-feed-tag-uri the-site-url founded coll-names))

  ;; Get first author as feed author
  (define author (parse-author (car authors)))

  ;; Gather pages from all specified collections
  (define all-page-links
    (append*
     (for/list ([coll-name (in-list coll-names)])
       (get-collection coll-name))))

  ;; Filter for feed-eligible pages (exclude drafts and pages without dates)
  (define filtered-pages (filter-feed-pages all-page-links))

  ;; Convert to feed items
  (define feed-items
    (for/list ([pl (in-list filtered-pages)])
      (page-link->feed-item pl feed-tag-uri the-site-url author render-fn)))

  ;; Create the feed
  (define the-feed (feed feed-tag-uri the-site-url the-site-title feed-items))

  ;; Generate feed URL
  (define feed-url (url-join the-site-url filename))

  ;; Express as XML
  (express-xml the-feed feed-format feed-url))

;; Resolve a module binding spec to a function
(define (resolve-module-binding spec)
  (define mod-path (car spec))
  (define binding (cadr spec))
  (dynamic-require mod-path binding))

;; ---------------------------------------------------------------------------
;; Build Integration

;; Generate all feeds for a site and write them to the output folder.
(define (generate-feeds! site info)
  (define feeds (site-feeds site))
  (when (and feeds (not (null? feeds)))
    (define root-dir (site-root site))
    (define output-dir (build-path root-dir (site-output-folder site)))

    (for ([feed-cfg (in-list feeds)])
      (define filename (feed-config-filename feed-cfg))
      (define output-path (build-path output-dir filename))
      (define xml-str (generate-feed site info feed-cfg))

      (make-parent-directory* output-path)
      (call-with-output-file output-path
        (lambda (out) (display xml-str out))
        #:exists 'replace)

      (log-camp-info "generated feed: ~a" filename))))
