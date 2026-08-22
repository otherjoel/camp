#lang racket/base

;; Observable preferences that persist to a preferences file

(require racket/file
         racket/gui/easy
         racket/gui/easy/operator
         racket/list
         racket/match
         racket/path
         setup/getinfo)

(provide @sites
         @site-selection
         @editor
         @vim-mode
         @fill-column
         @line-numbers?
         update-pref!
         remove-from-pref!
         add-to-pref!
         discover-installed-sites
         site-spec->display-name)

;; ============================================================================
;; Preference file location

(define prefs-file
  (build-path (find-system-path 'pref-dir) "camp-prefs.rktd"))

;; ============================================================================
;; Observable preference factory

(define (make-@pref pref-key [init null])
  (define/obs @pref
    (get-preference pref-key
                    (λ () (begin0 init
                                  (put-preferences (list pref-key) (list init) #f prefs-file)))
                    'timestamp
                    prefs-file
                    #:use-lock? #f))
  (obs-observe! @pref
                (λ (v) (put-preferences (list pref-key) (list v) #f prefs-file)))
  @pref)

(define (update-pref! @pref v proc)
  (@pref . <~ . (λ (old-v) (proc v old-v))))

(define (add-to-pref! @pref v)
  (update-pref! @pref v
                (λ (new old)
                  (if (member new old)
                      old
                      (cons new old)))))

(define (remove-from-pref! @pref v)
  (update-pref! @pref v
                (λ (to-remove lst)
                  (remove to-remove lst equal?))))

;; ============================================================================
;; Site discovery

(define (discover-installed-sites)
  (define all-dirs (find-relevant-directories '(camp-site) 'all-available))
  (for/list ([dir (in-list all-dirs)])
    (define get-info (get-info/full dir))
    (when get-info
      (define camp-site (get-info 'camp-site (λ () #f)))
      (define collection (get-info 'collection (λ () #f)))
      (when (and camp-site collection)
        (string->symbol collection)))))

;; ============================================================================
;; Display helpers

(define (site-spec->display-name spec)
  (cond
    [(symbol? spec) (symbol->string spec)]
    [(path? spec) (path->string (file-name-from-path spec))]
    [(string? spec) (let ([p (string->path spec)])
                      (path->string (file-name-from-path p)))]
    [else "Unknown"]))

;; ============================================================================
;; Predefined preferences

(define @sites (make-@pref 'sites '()))

(define @site-selection
  (make-@pref 'site-selection
              (match (obs-peek @sites)
                ['() #f]
                [(list* first _) first])))

(define @editor (make-@pref 'editor ""))

(define @vim-mode (make-@pref 'vim-mode #f))

(define @fill-column (make-@pref 'fill-column 80))

(define @line-numbers? (make-@pref 'line-numbers #t))
