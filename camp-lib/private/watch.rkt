#lang racket/base

;; File system watching for dev server

(require racket/file
         racket/list
         racket/match
         racket/path
         racket/string
         "structs.rkt"
         "collections.rkt"
         "log.rkt")

(provide get-watch-paths
         start-watcher!
         path-change-type)

;; ---------------------------------------------------------------------------
;; Path Change Types

(define (path-change-type changed-path site site-config-path)
  (define simplified (simplify-path changed-path))
  (define config-simplified (simplify-path site-config-path))
  (define root (site-root site))
  (define static-dir (build-path root (site-static-folder site)))

  (cond
    ;; Site config file
    [(equal? simplified config-simplified) 'config]

    ;; Static folder (path starts with static dir)
    [(path-prefix? simplified static-dir) 'static]

    ;; Source file (matches source extension in a collection directory)
    [(is-source-file? simplified site) 'source]

    ;; .rkt file
    [(path-has-extension? simplified #".rkt") 'rkt]

    [else 'unknown]))

(define (path-prefix? path prefix)
  (define path-parts (explode-path (simplify-path path)))
  (define prefix-parts (explode-path (simplify-path prefix)))
  (and (>= (length path-parts) (length prefix-parts))
       (equal? (take path-parts (length prefix-parts)) prefix-parts)))

(define (is-source-file? path site)
  (define source-ext (site-sources site))
  (define root (site-root site))
  (and (path-has-extension? path (string->bytes/utf-8 source-ext))
       (for/or ([coll (in-list (site-collections site))])
         (define source-dir (build-path root (source-pattern->directory (collection-source coll))))
         (path-prefix? path source-dir))))

;; ---------------------------------------------------------------------------
;; Watch Path Enumeration

(define (get-watch-paths site site-config-path)
  (define root (site-root site))
  (define source-ext (site-sources site))
  (define static-dir (build-path root (site-static-folder site)))
  (define output-dir (build-path root (site-output-folder site)))

  (remove-duplicates
   (filter file-or-directory-exists?
           (append
            (list (simplify-path site-config-path))
            (get-source-paths site root source-ext)
            (get-static-paths static-dir)
            (get-render-module-paths site root)
            (get-root-rkt-files root output-dir)))))

(define (file-or-directory-exists? p)
  (or (file-exists? p) (directory-exists? p)))

(define (get-source-paths site root source-ext)
  (for*/list ([coll (in-list (site-collections site))]
              [path (in-list (get-collection-source-paths root coll source-ext))])
    path))

(define (get-collection-source-paths root coll source-ext)
  (define source-dir (build-path root (source-pattern->directory (collection-source coll))))
  (if (directory-exists? source-dir)
      (cons source-dir (find-sources source-dir source-ext))
      '()))

(define (get-static-paths static-dir)
  (if (directory-exists? static-dir)
      (cons static-dir
            (for/list ([item (in-directory static-dir)])
              item))
      '()))

(define (get-render-module-paths site root)
  (define render-specs
    (append
     (filter-map collection-render-with (site-collections site))
     (map feed-config-render-with (site-feeds site))
     (if (site-default-render site)
         (list (site-default-render site))
         '())
     (if (site-element-fallback site)
         (list (site-element-fallback site))
         '())))

  (filter-map
   (λ (spec)
     (define mod-path (car spec))
     (resolve-local-module-path mod-path root))
   render-specs))

(define (resolve-local-module-path mod-path root)
  (cond
    [(symbol? mod-path)
     (define parts (string-split (symbol->string mod-path) "/"))
     (define file-name (string-append (last parts) ".rkt"))
     (define local-path (build-path root file-name))
     (if (file-exists? local-path)
         local-path
         #f)]
    [(string? mod-path)
     (define local-path (build-path root mod-path))
     (if (file-exists? local-path)
         local-path
         #f)]
    [else #f]))

(define (get-root-rkt-files root output-dir)
  (if (directory-exists? root)
      (for/list ([item (in-list (directory-list root #:build? #t))]
                 #:when (and (file-exists? item)
                             (path-has-extension? item #".rkt")
                             ;; Exclude output directory
                             (not (path-prefix? item output-dir))))
        item)
      '()))

;; ---------------------------------------------------------------------------
;; File Watcher

(define (start-watcher! paths on-change #:debounce-ms [debounce-ms 1000])
  (define stop-flag (box #f))
  (define last-rebuild-end-time (box 0))

  (define watcher-thread
    (thread
     (λ ()
       (let loop ()
         (unless (unbox stop-flag)
           ;; Re-enumerate paths each iteration to catch new files
           (define current-paths (filter file-or-directory-exists? paths))

           (when (null? current-paths)
             (log-camp-warning "no paths to watch")
             (sleep 1)
             (loop))

           ;; Create filesystem change events for all paths
           (define evts
             (for/list ([p (in-list current-paths)])
               (filesystem-change-evt p)))

           ;; Wait for any change (with break support)
           (define result
             (with-handlers ([exn:break? (λ (e) 'break)])
               (apply sync/enable-break
                      (for/list ([p (in-list current-paths)]
                                 [evt (in-list evts)])
                        (handle-evt evt (λ (_) p))))))

           ;; Cancel all events
           (for-each filesystem-change-evt-cancel evts)

           ;; Handle result
           (match result
             ['break (void)]  ; Stop on break
             [(? path? changed-path)
              ;; Debounce: check if enough time has passed since last rebuild COMPLETED
              (define now (current-inexact-milliseconds))
              (define last-end (unbox last-rebuild-end-time))
              (cond
                [(> (- now last-end) debounce-ms)
                 ;; Trigger rebuild
                 (on-change changed-path)
                 ;; Record when rebuild finished (not when it started)
                 (set-box! last-rebuild-end-time (current-inexact-milliseconds))]
                [else
                 ;; Within debounce window, skip but log
                 (log-camp-debug "debounce: skipping event for ~a" changed-path)])
              (loop)]
             [_ (loop)]))))))

  ;; Return stop procedure
  (λ ()
    (set-box! stop-flag #t)
    (when (thread-running? watcher-thread)
      (break-thread watcher-thread)
      (sync/timeout 0.5 watcher-thread))))
