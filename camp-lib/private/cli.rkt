#lang racket/base

;; Camp CLI - raco camp command dispatcher
;;
;; Usage:
;;   raco camp build [--fresh] [--verbose] [site-path]
;;   raco camp serve [--port N] [--no-watch]  (not yet implemented)
;;   raco camp deploy                          (not yet implemented)
;;   raco camp new <name>                      (not yet implemented)

(require racket/cmdline
         racket/file
         racket/format
         racket/match
         racket/path
         racket/vector
         "main.rkt"
         "build.rkt"
         "structs.rkt"
         "output.rkt")

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
       ["serve" (not-implemented "serve")]
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
  (displayln "  build [--fresh] [--verbose] [site-path]  Build the site")
  (displayln "  serve [--port N] [--no-watch] Start dev server (not yet implemented)")
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

  (define remaining
    (command-line
     #:program "raco camp build"
     #:argv args
     #:once-each
     [("--fresh") "Clear output folder before building"
                  (set! fresh? #t)]
     [("--verbose" "-v") "Show detailed output"
                         (set! verbose? #t)]
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

  ;; Collect pass
  (define-values (info collect-ms)
    (with-timing
      (with-handlers ([exn:fail?
                       (λ (e)
                         (print-error "collecting" (exn-message e))
                         (exit 1))])
        (collect site))))

  (define page-count (length (site-info-pages info)))
  (print-phase-line "Collect"
                    (format-count-desc page-count "page" coll-count "collection")
                    collect-ms)

  ;; Build pass
  (define-values (_ build-ms)
    (with-timing
      (with-handlers ([exn:fail?
                       (λ (e)
                         (print-error "building" (exn-message e))
                         (exit 1))])
        (build! site info))))

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

  ;; Done
  (define total-ms (- (current-inexact-monotonic-milliseconds) total-start))
  (displayln "")
  (displayln (~a "  " (green "✓") " " (bold (~a "Done in " (format-duration total-ms)))))
  (displayln ""))

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
