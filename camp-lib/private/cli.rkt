#lang racket/base

;; Camp CLI - raco camp command dispatcher
;;
;; Usage:
;;   raco camp build [--fresh] [--verbose] [site-path]
;;   raco camp serve [--port N] [--no-watch] [site-path]
;;   raco camp deploy                          (not yet implemented)
;;   raco camp new <name>                      (not yet implemented)

(require racket/cmdline
         racket/file
         racket/format
         racket/list
         racket/logging
         racket/match
         racket/path
         racket/string
         racket/vector
         "main.rkt"
         "build.rkt"
         "serve.rkt"
         "structs.rkt"
         "output.rkt"
         "log.rkt")

(provide main)

;; ---------------------------------------------------------------------------
;; Output line width for right-aligned timing
(define LINE-WIDTH 60)

;; ---------------------------------------------------------------------------
;; Main entry point

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
       ["deploy" (not-implemented "deploy")]
       ["new" (not-implemented "new")]
       ["help" (show-usage)]
       [_ (eprintf "Unknown command: ~a\n" cmd)
          (show-usage)
          (exit 1)])]))

(define (not-implemented cmd)
  (eprintf "Command '~a' is not yet implemented.\n" cmd)
  (exit 1))

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
  (displayln "  deploy                        Run deploy script (not yet implemented)")
  (displayln "  new <name>                    Create new site (not yet implemented)")
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

  ;; Determine site path
  (define resolved-path
    (cond
      [remaining remaining]
      [(file-exists? "site.rkt") "site.rkt"]
      [else
       (eprintf "Error: No site.rkt found in current directory.\n")
       (eprintf "Specify a path: raco camp build <site-path>\n")
       (exit 1)]))

  ;; Track total build time
  (define total-start (current-inexact-monotonic-milliseconds))

  ;; Mutable list to collect warnings during build
  (define warnings '())
  (define (collect-warning! vec)
    ;; vec is #(level message data topic)
    (set! warnings (cons (vector-ref vec 1) warnings)))

  ;; Print header
  (displayln "")
  (displayln (bold "camp build"))
  (displayln "")

  ;; Load site
  (define-values (site load-ms)
    (with-timing
      (with-handlers ([exn:fail?
                       (λ (e)
                         (print-error "loading site" (exn-message e))
                         (exit 1))])
        (load-site resolved-path))))

  (when verbose?
    (print-phase-line "Load" (~a "from " resolved-path) load-ms))

  ;; Handle --fresh: clear output folder
  (when fresh?
    (define output-dir (build-path (site-root site) (site-output-folder site)))
    (when (directory-exists? output-dir)
      (delete-directory/files output-dir)
      (when verbose?
        (print-phase-line "Clean" "cleared output folder" #f))))

  ;; Count things before we build
  (define coll-count (length (site-collections site)))
  (define feed-count (length (site-feeds site)))
  (define static-dir (build-path (site-root site) (site-static-folder site)))
  (define static-count (count-files-in-directory static-dir))

  ;; Collect pass (with warning interception)
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

  ;; Build pass (with warning interception)
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

  ;; Feed info (feeds are generated as part of build!)
  (when (> feed-count 0)
    (print-phase-line "Feeds"
                      (pluralize feed-count "feed")
                      #f))

  ;; Static info
  (when (> static-count 0)
    (print-phase-line "Static"
                      (pluralize static-count "file")
                      #f))

  ;; Display warnings if any
  (define warning-count (length warnings))
  (when (> warning-count 0)
    (define root-str (path->string (site-root site)))
    ;; Ensure root-str ends with / for clean replacement
    (define root-prefix
      (if (string-suffix? root-str "/")
          root-str
          (string-append root-str "/")))
    (displayln "")
    (displayln (~a "  " (yellow "⚠") " " (bold (pluralize warning-count "warning")) ":"))
    (for ([w (in-list (reverse warnings))])
      ;; Convert absolute paths to relative by removing the site root prefix
      (define display-warning (string-replace w root-prefix ""))
      (displayln (~a "    • " display-warning))))

  ;; Done
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

  ;; Determine site path
  (define resolved-path
    (cond
      [remaining remaining]
      [(file-exists? "site.rkt") "site.rkt"]
      [else
       (eprintf "Error: No site.rkt found in current directory.\n")
       (eprintf "Specify a path: raco camp serve <site-path>\n")
       (exit 1)]))

  ;; Load site to get output folder
  (define site
    (with-handlers ([exn:fail?
                     (λ (e)
                       (print-error "loading site" (exn-message e))
                       (exit 1))])
      (load-site resolved-path)))

  (define output-dir (build-path (site-root site) (site-output-folder site)))

  (unless (directory-exists? output-dir)
    (eprintf "Error: Output folder does not exist: ~a\n" output-dir)
    (eprintf "Run 'raco camp build' first to generate the site.\n")
    (exit 1))

  ;; Set up log receiver to display request logs
  (define log-receiver (make-log-receiver camp-logger 'info))

  ;; Log display thread - reads from receiver and colorizes output
  (define log-thread
    (thread
     (lambda ()
       (let loop ()
         (define vec (sync log-receiver))
         ;; vec is #(level message data topic)
         (define raw-msg (vector-ref vec 1))
         ;; Strip "camp: " prefix added by define-logger
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

  ;; Start server
  (define shutdown (start-server output-dir #:port port #:watch? watch? #:log-format log-format))

  ;; Keep running until interrupted
  (with-handlers ([exn:break? (λ (e)
                                (displayln "")
                                (displayln (~a "  " (dim "Shutting down...")))
                                (shutdown)
                                (kill-thread log-thread)
                                (displayln (~a "  " (green "✓") " Server stopped"))
                                (displayln ""))])
    (sync never-evt)))

;; Colorize modern log format: "HH:MM:SS METHOD PATH STATUS"
;; Returns colorized string
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

;; Format status code with color and glyph
(define (format-status-code code)
  (cond
    [(not code) (dim "???")]
    [(< code 300) (~a (green "●") " " (green (~a code)))]   ; 2xx success
    [(< code 400) (~a (dim "●") " " (dim (~a code)))]       ; 3xx redirect
    [(< code 500) (~a (yellow "●") " " (yellow (~a code)))] ; 4xx client error
    [else (~a (red "●") " " (red (~a code)))]))

;; ---------------------------------------------------------------------------
;; Output helpers

;; Print a phase line:  ● Phase    description          timing
(define (print-phase-line phase-name desc timing-ms)
  (define bullet (cyan "●"))
  (define phase (~a phase-name #:min-width 10))
  (define description (dim desc))
  (define timing (if timing-ms (dim (format-duration timing-ms)) ""))

  ;; Calculate spacing for right-aligned timing
  ;; Format: "  ● Phase     desc                 timing"
  (define prefix (~a "  " bullet " " phase description))
  (define prefix-len (+ 2 1 1 10 (string-length desc)))  ; account for non-color chars
  (define padding (max 1 (- LINE-WIDTH prefix-len (string-length (format-duration (or timing-ms 0))))))

  (displayln (~a "  " bullet " " phase description
                 (make-string padding #\space)
                 timing)))

;; Print an error message
(define (print-error context msg)
  (eprintf "\n  ~a Error ~a: ~a\n" (red "✗") context msg))

;; Format a count description like "9 pages in 2 collections"
(define (format-count-desc count1 noun1 count2 noun2)
  (~a (pluralize count1 noun1) " in " (pluralize count2 noun2)))

;; Pluralize: "1 page" vs "2 pages"
(define (pluralize n noun)
  (~a n " " noun (if (= n 1) "" "s")))

;; Entry point when run as raco command
;; This runs when the module is required by raco
(main)
