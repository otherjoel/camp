#lang racket/base

;; Camp CLI - raco camp command dispatcher

(require racket/cmdline
         racket/file
         racket/format
         racket/list
         racket/logging
         racket/match
         racket/path
         racket/rerequire
         racket/string
         racket/system
         racket/vector
         "main.rkt"
         "build.rkt"
         "serve.rkt"
         "watch.rkt"
         "structs.rkt"
         "output.rkt"
         "log.rkt"
         "new-site.rkt")

(provide main)

;; ---------------------------------------------------------------------------
;; Main Entry Point

(define LINE-WIDTH 60)

(define (main)
  (define args (current-command-line-arguments))
  (cond
    [(zero? (vector-length args))
     (show-usage)]
    [else
     (define cmd (vector-ref args 0))
     (define cmd-args (vector-drop args 1))
     (match cmd
       ["build" (run-build cmd-args)]
       ["serve" (run-serve cmd-args)]
       ["deploy" (run-deploy cmd-args)]
       ["new" (run-new cmd-args)]
       ["help" (show-usage)]
       [_ (eprintf "Unknown command: ~a\n" cmd)
          (show-usage)
          (exit 1)])]))

(define (show-usage)
  (displayln "Usage: raco camp <command> [options]")
  (displayln "")
  (displayln "Commands:")
  (displayln "  build [options] [site-path]   Build the site")
  (displayln "    --fresh                     Clear output folder before building")
  (displayln "    --verbose, -v               Show detailed output")
  (displayln "    --drama                     Treat warnings as errors (non-zero exit)")
  (displayln "  serve [options] [site-path]   Start dev server")
  (displayln "    --port N                    Port number (default: 8000)")
  (displayln "    --no-watch                  Disable file watching")
  (displayln "    --apache-log                Use Apache combined log format")
  (displayln "  deploy [site-path]            Run deploy script")
  (displayln "  new <name>                    Create new site from template")
  (displayln "  help                          Show this help")
  (displayln "")
  (displayln "If site-path is not specified, looks for site.rkt in current directory."))

;; ---------------------------------------------------------------------------
;; Build command

(define (run-build args)
  (define fresh? #f)
  (define verbose? #f)
  (define warnings-as-errors? #f)

  (define remaining
    (command-line
     #:program "raco camp build"
     #:argv args
     #:once-each
     [("--fresh") "Clear output folder before building"
                  (set! fresh? #t)]
     [("--verbose" "-v") "Show detailed output"
                         (set! verbose? #t)]
     [("--drama") "Treat warnings as errors (non-zero exit)"
                 (set! warnings-as-errors? #t)]
     #:args ([path #f])
     path))

  (define resolved-path
    (cond
      [remaining remaining]
      [(file-exists? "site.rkt") "site.rkt"]
      [else
       (eprintf "Error: No site.rkt found in current directory.\n")
       (eprintf "Specify a path: raco camp build <site-path>\n")
       (exit 1)]))

  (define total-start (current-inexact-monotonic-milliseconds))

  (define warnings '())
  (define (collect-warning! vec)
    (set! warnings (cons (vector-ref vec 1) warnings)))

  (displayln "")
  (displayln (bold "camp build"))
  (displayln "")

  (define-values (site load-ms)
    (with-timing
      (with-handlers ([exn:fail?
                       (λ (e)
                         (print-error "loading site" (exn-message e))
                         (exit 1))])
        (load-site resolved-path))))

  (when verbose?
    (print-phase-line "Load" (~a "from " resolved-path) load-ms))

  (when fresh?
    (define output-dir (build-path (site-root site) (site-output-folder site)))
    (when (directory-exists? output-dir)
      (delete-directory/files output-dir)
      (when verbose?
        (print-phase-line "Clean" "cleared output folder" #f))))

  (define coll-count (length (site-collections site)))
  (define feed-count (length (site-feeds site)))
  (define static-dir (build-path (site-root site) (site-static-folder site)))
  (define static-count (count-files-in-directory static-dir))

  (define-values (info collect-ms)
    (with-timing
      (with-intercepted-logging
        collect-warning!
        (λ ()
          (with-handlers ([exn:fail?
                           (λ (e)
                             (print-error "collecting" (exn-message e))
                             (exit 1))])
            (collect site)))
        #:logger camp-logger
        'warning)))

  (define page-count (length (site-info-pages info)))
  (print-phase-line "Collect"
                    (format-count-desc page-count "page" coll-count "collection")
                    collect-ms)

  (define-values (_ build-ms)
    (with-timing
      (with-intercepted-logging
        collect-warning!
        (λ ()
          (with-handlers ([exn:fail?
                           (λ (e)
                             (print-error "building" (exn-message e))
                             (exit 1))])
            (build! site info)))
        #:logger camp-logger
        'warning)))

  (print-phase-line "Build"
                    (pluralize page-count "page")
                    build-ms)

  (when (> feed-count 0)
    (print-phase-line "Feeds"
                      (pluralize feed-count "feed")
                      #f))

  (when (> static-count 0)
    (print-phase-line "Static"
                      (pluralize static-count "file")
                      #f))

  (define warning-count (length warnings))
  (when (> warning-count 0)
    (define root-str (path->string (site-root site)))
    (define root-prefix
      (if (string-suffix? root-str "/")
          root-str
          (string-append root-str "/")))
    (displayln "")
    (displayln (~a "  " (yellow "⚠") " " (bold (pluralize warning-count "warning")) ":"))
    (for ([w (in-list (reverse warnings))])
      (define display-warning (string-replace w root-prefix ""))
      (displayln (~a "    • " display-warning))))

  (define total-ms (- (current-inexact-monotonic-milliseconds) total-start))
  (displayln "")
  (if (and warnings-as-errors? (> warning-count 0))
      (begin
        (displayln (~a "  " (red "✗") " " (bold (~a "Failed in " (format-duration total-ms)))
                       " " (dim "(warnings treated as errors)")))
        (displayln "")
        (exit 1))
      (begin
        (displayln (~a "  " (green "✓") " " (bold (~a "Done in " (format-duration total-ms)))))
        (displayln ""))))

;; ---------------------------------------------------------------------------
;; Serve command

(define (run-serve args)
  (define port 8000)
  (define watch? #t)
  (define log-format 'modern)

  (define remaining
    (command-line
     #:program "raco camp serve"
     #:argv args
     #:once-each
     [("--port") p "Port number (default: 8000)"
                 (define n (string->number p))
                 (unless (and n (exact-positive-integer? n) (<= n 65535))
                   (eprintf "Error: Invalid port number: ~a\n" p)
                   (exit 1))
                 (set! port n)]
     [("--no-watch") "Disable file watching"
                     (set! watch? #f)]
     [("--apache-log") "Use Apache combined log format"
                       (set! log-format 'apache)]
     #:args ([path #f])
     path))

  (define site-config-path
    (simplify-path
     (path->complete-path
      (cond
        [remaining remaining]
        [(file-exists? "site.rkt") "site.rkt"]
        [else
         (eprintf "Error: No site.rkt found in current directory.\n")
         (eprintf "Specify a path: raco camp serve <site-path>\n")
         (exit 1)]))))

  (define current-site #f)

  (define (reload-site!)
    (set! current-site (load-site site-config-path)))

  (with-handlers ([exn:fail?
                   (λ (e)
                     (print-error "loading site" (exn-message e))
                     (exit 1))])
    (reload-site!))

  (define (get-output-dir)
    (build-path (site-root current-site) (site-output-folder current-site)))

  (define (get-static-dir)
    (build-path (site-root current-site) (site-static-folder current-site)))

  (define manifest-path
    (if watch?
        (make-temporary-file "camp-static-~a")
        #f))

  (define (do-rebuild!)
    (with-handlers ([exn:fail?
                     (λ (e)
                       (displayln (~a "  " (red "✗") " Build error: " (exn-message e)))
                       #f)])
      (define info (collect current-site))
      (build! current-site info)
      #t))

  (define (do-static-sync!)
    (with-handlers ([exn:fail?
                     (λ (e)
                       (displayln (~a "  " (red "✗") " Static sync error: " (exn-message e)))
                       #f)])
      (sync-static-files (get-static-dir) (get-output-dir) manifest-path)
      #t))

  (define (rerequire-render-modules!)
    (for ([coll (in-list (site-collections current-site))])
      (define spec (collection-render-with coll))
      (when spec
        (with-handlers ([exn:fail? void])
          (dynamic-rerequire (car spec)))))
    (for ([feed (in-list (site-feeds current-site))])
      (with-handlers ([exn:fail? void])
        (dynamic-rerequire (car (feed-config-render-with feed)))))
    (when (site-default-render current-site)
      (with-handlers ([exn:fail? void])
        (dynamic-rerequire (car (site-default-render current-site))))))

  (unless (directory-exists? (get-output-dir))
    (displayln "")
    (displayln (~a "  " (dim "Output folder not found, building...")))
    (unless (do-rebuild!)
      (exit 1))
    (displayln (~a "  " (green "✓") " Initial build complete"))
    (displayln ""))

  (define log-receiver (make-log-receiver camp-logger 'info))

  (define log-thread
    (thread
     (lambda ()
       (let loop ()
         (define vec (sync log-receiver))
         (define raw-msg (vector-ref vec 1))
         (define msg (if (string-prefix? raw-msg "camp: ")
                         (substring raw-msg 6)
                         raw-msg))
         (define colorized
           (if (eq? log-format 'modern)
               (colorize-modern-log msg)
               msg))
         (displayln (~a "  " colorized))
         (flush-output)
         (loop)))))

  (define shutdown-server
    (start-server (get-output-dir) #:port port #:watch? watch? #:log-format log-format))

  (define stop-watcher (box void))

  (define (current-time-str)
    (define now (seconds->date (current-seconds)))
    (define (zpad n) (if (< n 10) (~a "0" n) (~a n)))
    (~a (zpad (date-hour now)) ":" (zpad (date-minute now)) ":" (zpad (date-second now))))

  (define (handle-change changed-path)
    (define rel-path (make-relative-path changed-path))
    (define change-type (path-change-type changed-path current-site site-config-path))

    (displayln "")
    (displayln (~a "  " (dim (current-time-str)) "  " (cyan "●") " " rel-path " " (dim "changed")))

    (define-values (result rebuild-ms)
      (with-timing
        (case change-type
          [(config)
           (displayln (~a "  " (dim "Reloading site config...")))
           ((unbox stop-watcher))
           (with-handlers ([exn:fail?
                            (λ (e)
                              (displayln (~a "  " (red "✗") " Config error: " (exn-message e)))
                              #f)])
             (reload-site!)
             (start-watching!)
             (do-rebuild!))]

          [(static)
           (displayln (~a "  " (dim "Syncing static files...")))
           (do-static-sync!)]

          [(rkt)
           (displayln (~a "  " (dim "Reloading modules...")))
           (rerequire-render-modules!)
           (displayln (~a "  " (dim "Rebuilding...")))
           (do-rebuild!)]

          [(source)
           (displayln (~a "  " (dim "Rebuilding...")))
           (do-rebuild!)]

          [else
           (displayln (~a "  " (dim "Rebuilding...")))
           (do-rebuild!)])))

    (when result
      (displayln (~a "  " (green "✓") " Done " (dim (format-duration rebuild-ms))))))

  (define (make-relative-path p)
    (define root (site-root current-site))
    (define rel (find-relative-path root p))
    (if (equal? rel p)
        (path->string (file-name-from-path p))
        (path->string rel)))

  (define (start-watching!)
    (define paths (get-watch-paths current-site site-config-path))
    (set-box! stop-watcher
              (start-watcher! paths handle-change)))

  (when watch?
    (start-watching!))

  (define (cleanup!)
    (displayln "")
    (displayln (~a "  " (dim "Shutting down...")))
    ((unbox stop-watcher))
    (shutdown-server)
    (kill-thread log-thread)
    (when (and manifest-path (file-exists? manifest-path))
      (delete-file manifest-path))
    (displayln (~a "  " (green "✓") " Server stopped"))
    (displayln ""))

  (with-handlers ([exn:break? (λ (e) (cleanup!))])
    (sync never-evt)))

;; ---------------------------------------------------------------------------
;; Deploy command

(define (run-deploy args)
  (define remaining
    (command-line
     #:program "raco camp deploy"
     #:argv args
     #:args ([path #f])
     path))

  (define resolved-path
    (cond
      [remaining remaining]
      [(file-exists? "site.rkt") "site.rkt"]
      [else
       (eprintf "Error: No site.rkt found in current directory.\n")
       (eprintf "Specify a path: raco camp deploy <site-path>\n")
       (exit 1)]))

  (define site
    (with-handlers ([exn:fail?
                     (λ (e)
                       (eprintf "Error loading site: ~a\n" (exn-message e))
                       (exit 1))])
      (load-site resolved-path)))

  (define deploy-script (site-deploy-script site))

  (unless deploy-script
    (eprintf "Error: No deploy-script configured in site configuration.\n")
    (eprintf "Add deploy-script = \"./deploy.sh\" to your site.rkt\n")
    (exit 1))

  (define root (site-root site))
  (define script-path (simplify-path (build-path root deploy-script)))

  (unless (file-exists? script-path)
    (eprintf "Error: Deploy script not found: ~a\n" script-path)
    (exit 1))

  (define output-dir
    (path->string (simplify-path (build-path root (site-output-folder site)))))

  (displayln "")
  (displayln (bold "camp deploy"))
  (displayln "")
  (displayln (~a "  " (dim "Running") " " deploy-script " " output-dir))
  (displayln "")

  (define exit-code
    (parameterize ([current-directory root])
      (apply system*/exit-code script-path (list output-dir))))

  (displayln "")
  (if (zero? exit-code)
      (displayln (~a "  " (green "✓") " " (bold "Deploy complete")))
      (displayln (~a "  " (red "✗") " " (bold "Deploy failed") " (exit code " exit-code ")")))
  (displayln "")

  (exit exit-code))

;; ---------------------------------------------------------------------------
;; New command

(define (run-new args)
  (define name
    (command-line
     #:program "raco camp new"
     #:argv args
     #:args (name)
     name))

  ;; Validate name is a valid Racket identifier (roughly)
  (unless (regexp-match? #rx"^[a-z][a-z0-9-]*$" name)
    (eprintf "Error: Invalid site name '~a'\n" name)
    (eprintf "Name must start with a letter and contain only lowercase letters, numbers, and hyphens.\n")
    (exit 1))

  (define target-dir (build-path (current-directory) name))

  ;; Check if directory already exists
  (when (or (directory-exists? target-dir)
            (file-exists? target-dir))
    (eprintf "Error: '~a' already exists.\n" name)
    (eprintf "Choose a different name or remove the existing directory.\n")
    (exit 1))

  (displayln "")
  (displayln (bold "camp new"))
  (displayln "")

  (with-handlers ([exn:fail?
                   (λ (e)
                     (eprintf "  ~a Error: ~a\n" (red "✗") (exn-message e))
                     (exit 1))])
    (create-new-site target-dir name))

  (displayln (~a "  " (green "✓") " Created " (bold name)))
  (displayln "")
  (displayln (~a "  " (dim "Next steps:")))
  (displayln (~a "    cd " name))
  (displayln "    raco pkg install")
  (displayln "    raco camp build")
  (displayln "    raco camp serve")
  (displayln ""))

;; ---------------------------------------------------------------------------
;; Serve log formatting

(define (colorize-modern-log msg)
  (define parts (string-split msg " "))
  (cond
    [(>= (length parts) 4)
     (define time (car parts))
     (define method (cadr parts))
     (define status-str (last parts))
     (define path (string-join (drop-right (cddr parts) 1) " "))
     (define status (string->number status-str))
     (~a (dim time) "  "
         (cyan (~a method #:min-width 4)) "  "
         (~a path #:min-width 30) "  "
         (format-status-code status))]
    [else msg]))

(define (format-status-code code)
  (cond
    [(not code) (dim "???")]
    [(< code 300) (~a (green "●") " " (green (~a code)))]
    [(< code 400) (~a (dim "●") " " (dim (~a code)))]
    [(< code 500) (~a (yellow "●") " " (yellow (~a code)))]
    [else (~a (red "●") " " (red (~a code)))]))

;; ---------------------------------------------------------------------------
;; Output Helpers

(define (print-phase-line phase-name desc timing-ms)
  (define bullet (cyan "●"))
  (define phase (~a phase-name #:min-width 10))
  (define description (dim desc))
  (define timing (if timing-ms (dim (format-duration timing-ms)) ""))

  (define prefix (~a "  " bullet " " phase description))
  (define prefix-len (+ 2 1 1 10 (string-length desc)))
  (define padding (max 1 (- LINE-WIDTH prefix-len (string-length (format-duration (or timing-ms 0))))))

  (displayln (~a "  " bullet " " phase description
                 (make-string padding #\space)
                 timing)))

(define (print-error context msg)
  (eprintf "\n  ~a Error ~a: ~a\n" (red "✗") context msg))

(define (format-count-desc count1 noun1 count2 noun2)
  (~a (pluralize count1 noun1) " in " (pluralize count2 noun2)))

(define (pluralize n noun)
  (~a n " " noun (if (= n 1) "" "s")))

(main)
