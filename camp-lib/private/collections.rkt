#lang racket/base

(require racket/file
         racket/list
         racket/match
         racket/path
         racket/string
         punct/fetch
         punct/doc
         gregor
         "structs.rkt")

(provide source-pattern->directory
         is-source?
         find-sources
         in-sources
         path->slug
         parse-sort-value
         sort-pages)

;; ---------------------------------------------------------------------------
;; Source Pattern Handling

(define (source-pattern->directory source-pattern)
  (define p (string->path source-pattern))
  (define parts (explode-path p))
  (if (= 1 (length parts))
      (current-directory)
      (apply build-path (drop-right parts 1))))

;; ---------------------------------------------------------------------------
;; Source File Detection

(define (is-source? p source-extension)
  (and (path-has-extension? p (string->bytes/utf-8 source-extension))
       (not (regexp-match? #rx#"\\.#" (if (path? p) (path->bytes p) p)))))

;; ---------------------------------------------------------------------------
;; Source File Discovery

(define (find-sources root source-extension)
  (for/list ([p (in-list (directory-list root #:build? #t))]
             #:when (and (file-exists? p)
                         (is-source? p source-extension)))
    p))

;; ---------------------------------------------------------------------------
;; Slug Extraction

(define (path->slug p source-extension)
  (define as-path (if (path? p) p (string->path p)))
  (define filename (path->string (file-name-from-path as-path)))
  (define ext-pattern (regexp-quote source-extension))
  (regexp-replace (regexp (string-append ext-pattern "$")) filename ""))

;; ---------------------------------------------------------------------------
;; Source Iteration

(define (in-sources root source-extension)
  (define paths (find-sources root source-extension))
  (make-do-sequence
   (lambda ()
     (values
      (lambda (idx)
        (define p (list-ref paths idx))
        (define doc (get-doc p))
        (define slug (or (meta-ref doc 'slug)
                         (path->slug p source-extension)))
        (values p slug doc))
      add1
      0
      (lambda (idx) (< idx (length paths)))
      #f
      #f))))

;; ---------------------------------------------------------------------------
;; Page Sorting

(define (parse-sort-value val sort-key)
  (cond
    [(date? val) val]
    [(and (equal? sort-key "date") (string? val)) (iso8601->date val)]
    [else val]))

(define (sort-pages pages coll)
  (define sort-key (collection-sort-key coll))
  (define order (collection-order coll))
  (define coll-name (collection-name coll))

  (define pages-with-sort-vals
    (for/list ([page (in-list pages)])
      (match-define (list _ slug doc) page)
      (define raw-val (meta-ref doc (string->symbol sort-key)))
      (unless raw-val
        (error 'sort-pages
               "page \"~a\" in collection \"~a\" is missing required metadata key \"~a\""
               slug coll-name sort-key))
      (define sort-val (parse-sort-value raw-val sort-key))
      (cons sort-val page)))

  (define ascending? (equal? order "ascending"))

  (define (compare-values a b)
    (define (less-than? x y)
      (cond
        [(and (date? x) (date? y)) (date<? x y)]
        [(and (string? x) (string? y)) (string<? x y)]
        [(and (number? x) (number? y)) (< x y)]
        [else (string<? (format "~a" x) (format "~a" y))]))
    (if ascending?
        (less-than? a b)
        (less-than? b a)))

  (define sorted (sort pages-with-sort-vals compare-values #:key car))
  (map cdr sorted))
