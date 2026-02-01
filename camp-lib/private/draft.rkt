#lang racket/base

;; Draft post creation functionality

(require racket/date
         racket/file
         racket/format
         "structs.rkt"
         "collections.rkt"
         "xref.rkt")

(provide create-draft
         generate-draft-content
         title->filename)

;; ---------------------------------------------------------------------------
;; Content Generation

(define (generate-draft-content title date-str package-name)
  (define lang-line
    (if package-name
        (~a "#lang punct " package-name)
        "#lang punct"))
  (~a lang-line "\n"
      "\n"
      "---\n"
      "title: " title "\n"
      "date: " date-str "\n"
      "draft?: true\n"
      "---\n"
      "\n"))

;; ---------------------------------------------------------------------------
;; Filename Generation

(define (title->filename title source-extension)
  (~a (normalize-slug title) source-extension))

(define (unique-filename dir base-name extension)
  (define base-path (build-path dir (~a base-name extension)))
  (if (not (file-exists? base-path))
      base-path
      (let loop ([n 2])
        (define numbered-path (build-path dir (~a base-name "-" n extension)))
        (if (file-exists? numbered-path)
            (loop (add1 n))
            numbered-path))))

;; ---------------------------------------------------------------------------
;; Draft Creation

(define (create-draft site title #:collection [coll-name #f])
  (define collections (site-collections site))
  (when (null? collections)
    (error 'create-draft "site has no collections defined"))

  (define coll
    (if coll-name
        (or (findf (λ (c) (equal? (collection-name c) coll-name)) collections)
            (error 'create-draft "collection not found: ~a" coll-name))
        (car collections)))

  (define source-pattern (collection-source coll))
  (define source-dir (build-path (site-root site) (source-pattern->directory source-pattern)))
  (define source-extension (site-sources site))
  (define package-name (site-racket-collection site))

  (define slug (normalize-slug title))
  (define file-path (unique-filename source-dir slug source-extension))

  (define date-str (today-date-string))
  (define content (generate-draft-content title date-str package-name))

  (make-parent-directory* file-path)
  (call-with-output-file file-path
    (λ (out) (display content out))
    #:exists 'error)

  file-path)

;; ---------------------------------------------------------------------------
;; Date Formatting

(define (today-date-string)
  (define d (current-date))
  (format "~a-~a-~a"
          (date-year d)
          (~a (date-month d) #:min-width 2 #:pad-string "0" #:align 'right)
          (~a (date-day d) #:min-width 2 #:pad-string "0" #:align 'right)))
