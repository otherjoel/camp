#lang racket/base

;; Pure utility functions for site operations

(require camp
         punct/doc
         punct/fetch
         racket/list
         racket/path
         racket/string
         racket/vector
         camp/app/private/gui)

(provide (all-from-out camp) ; includes resolve-site-spec
         source-pattern->directory
         get-source-folders
         get-pages-in-folder)

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

(define (get-pages-in-folder site folder)
  (define root (site-root site))
  (define source-ext (site-sources site))
  (define pages
    (for/list ([coll (in-list (site-collections site))])
      (define source-dir
        (build-path root (source-pattern->directory (collection-source coll))))
      (if (equal? (simplify-path source-dir) (simplify-path folder))
          (get-pages-for-directory folder source-ext)
          '())))
  (list->vector (flatten pages)))

(define (get-pages-for-directory folder source-ext)
  (for/list ([p (in-list (directory-list folder #:build? #t))]
             #:when (and (file-exists? p)
                         (path-has-extension? p (string->bytes/utf-8 source-ext))
                         (not (regexp-match? #rx#"\\.#" (path->bytes p)))))
    (define doc (get-doc/safe p))
    (vector (or (meta-ref doc 'date) "")
            (or (meta-ref doc 'title) "Untitled")
            (path->string (file-name-from-path p))
            p)))

(define (get-doc/safe src-file)
  (with-handlers ([exn:fail?
                   (λ (e)
                     (log-msg "Warning: failed to load ~a: ~a"
                              (file-name-from-path src-file)
                              (exn-message e))
                     (document (hasheq 'here-path src-file 'title "Error") '() '()))])
    (get-doc src-file)))
