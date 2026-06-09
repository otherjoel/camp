#lang racket/base

;; Pure utility functions for site operations

(require camp
         (only-in camp/private/build output-path->url)
         punct/doc
         (only-in punct/fetch meta-ref)
         (only-in gregor iso8601->date date-provider?)
         racket/exn
         racket/list
         racket/path
         racket/string
         racket/vector
         camp/app/private/gui
         (only-in camp/private/collections is-source? path->slug load-doc))

(provide (all-from-out camp) ; includes resolve-site-spec
         source-pattern->directory
         get-source-folders
         get-pages-in-folder
         folder->collection)

;; ============================================================================
;; Path utilities

(define (source-pattern->directory source-pattern)
  (define p (string->path source-pattern))
  (define parts (explode-path p))
  (if (= 1 (length parts))
      "."
      (apply build-path (drop-right parts 1))))

;; ============================================================================
;; Site data extraction

(define (get-source-folders site)
  (define root (site-root site))
  (list->vector
   (filter directory-exists?
           (for/list ([coll (in-list (site-collections site))])
             (define source-pattern (collection-source coll))
             (build-path root (source-pattern->directory source-pattern))))))

(define (folder->collection site folder)
  (define root (site-root site))
  (for/first ([coll (in-list (site-collections site))]
              #:when (equal? (simplify-path (build-path root (source-pattern->directory (collection-source coll))))
                             (simplify-path folder)))
    coll))

(define (get-pages-in-folder site folder)
  (define root (site-root site))
  (define source-ext (site-sources site))
  (define pages
    (for/list ([coll (in-list (site-collections site))])
      (define source-dir
        (build-path root (source-pattern->directory (collection-source coll))))
      (if (equal? (simplify-path source-dir) (simplify-path folder))
          (get-pages-for-directory folder source-ext coll)
          '())))
  (list->vector (flatten pages)))

(define (get-page-date-val doc)
  (define raw (meta-ref doc 'date))
  (cond
    [(not raw) #f]
    [(date-provider? raw) raw]
    [(string? raw) (with-handlers ([exn:fail? (λ (_) #f)]) (iso8601->date raw))]
    [else #f]))

(define (get-pages-for-directory folder source-ext coll)
  (define output-pattern (collection-output-paths coll))
  (for/list ([p (in-list (directory-list folder #:build? #t))]
             #:when (and (file-exists? p)
                         (is-source? p source-ext)))
    (define doc (get-doc/safe p))
    (define slug (or (meta-ref doc 'slug) (path->slug p source-ext)))
    (define date-val (get-page-date-val doc))
    (define url
      (with-handlers ([exn:fail? (λ (_) (path->string (file-name-from-path p)))])
        (output-path->url (format-output-path output-pattern slug date-val (document-metas doc)))))
    (vector (or (meta-ref doc 'date) "")
            (or (meta-ref doc 'title) "Untitled")
            url
            p)))

(define (get-doc/safe src-file)
  (with-handlers ([exn:fail?
                   (λ (e)
                     (log-msg "Warning: failed to load ~a: ~a"
                              (file-name-from-path src-file)
                              (exn-message e))
                     (document (hasheq 'here-path src-file 'title "Error") '() '()))])
    (load-doc src-file)))
