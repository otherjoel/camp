#lang racket/base

;; Camp App - Main application window and logic

(require camp
         camp/build
         camp/serve
         net/sendurl
         racket/gui
         racket/gui/easy
         racket/gui/easy/operator
         racket/list
         racket/match
         racket/path
         racket/port
         racket/vector
         camp/app/private/settings
         camp/app/private/gui
         camp/app/private/site-utils)

(provide run-app)

;; ============================================================================
;; Helper functions

(define (vec-ref? v idx)
  (cond
    [(not (vector? v)) #f]
    [(zero? (vector-length v)) #f]
    [else (vector-ref v idx)]))

;; ============================================================================
;; Core observables
;;
;; The site observable is the foundation - everything else derives from it.

(define @site
  (obs-map @site-selection
           (λ (spec)
             (and spec
                  (with-handlers ([exn:fail?
                                   (λ (e)
                                     (render (?dialog (format "Error loading site: ~a" (exn-message e))))
                                     (remove-from-pref! @sites spec)
                                     #f)])
                    (define path (resolve-site-spec spec))
                    (cond
                      [(not path)
                       (render (?dialog (format "Cannot resolve site: ~a" spec)))
                       (remove-from-pref! @sites spec)
                       #f]
                      [else (load-site path)]))))))

(define @site-root (obs-map @site (λ (s) (and s (site-root s)))))

(define @output-folder
  (obs-map @site
           (λ (s) (and s (build-path (site-root s) (site-output-folder s))))))

;; ============================================================================
;; Build

(define (build-site!)
  (define site (obs-peek @site))
  (when site
    (log-msg "Building site...")
    (with-handlers ([exn:fail?
                     (λ (e)
                       (log-msg "Build error: ~a" (exn-message e)))])
      (define start-time (current-inexact-milliseconds))
      (define info (collect site))
      (build! site info)
      (define elapsed (- (current-inexact-milliseconds) start-time))
      (log-msg "Build complete (~ams)" (inexact->exact (round elapsed))))))

;; ============================================================================
;; Refresh triggers and derived data

(define/obs @refresh-counter 0)

(define (trigger-refresh!)
  (obs-update! @refresh-counter add1))

(define @folders
  (obs-combine
   (λ (site _counter)
     (when site (build-site!))
     (if site (get-source-folders site) (vector)))
   @site
   @refresh-counter))

(define @folder-selection (@ (vec-ref? (obs-peek @folders) 0)))

(define @source-doc-selection (@ ""))
(define-values (date-col title-col file-col fullpath) (values 0 1 2 3))
(define @source-sorting (@ (cons date-col string-ci>?)))

(define (sort-pages pages sorting)
  (define sort-key (car sorting))
  (vector-sort pages (λ (a b) ((cdr sorting) (vector-ref a sort-key) (vector-ref b sort-key)))))

(define @sources-in-folder
  (obs-combine
   (λ (site folder sorting _counter)
     (if (and site folder)
         (sort-pages (get-pages-in-folder site folder) sorting)
         (vector)))
   @site
   @folder-selection
   @source-sorting
   @refresh-counter))

(define/obs @stop-server-proc #f)

(define @startstop-caption
  (obs-map @stop-server-proc
           (λ (v) (if v "◼︎ Stop preview" "► Start preview"))))

;; ============================================================================
;; Server management

(define localhost-port 8000)

(define (mindful-demure-server-stop)
  (match (obs-peek @stop-server-proc)
    [(? procedure? stop)
     (log-msg "Stopping preview server")
     (stop)
     (@stop-server-proc . := . #f)
     #t]
    [_ #f]))

;; ============================================================================
;; Components: Site selection

(define (on-site-select v)
  (log-msg "Switched to site: ~a" v)
  (mindful-demure-server-stop)
  (@site-selection . := . v)
  (@folder-selection . := . (vec-ref? (obs-peek @folders) 0)))

(define :sites-choice
  (choice @sites on-site-select
          #:selection @site-selection
          #:choice->label site-spec->display-name
          #:label "Sites"))

;; ============================================================================
;; Components: Folder table

(define (src-folder->friendly p)
  (define name (path->string (last (explode-path p))))
  (string-append "📂 " (string-titlecase name)))

(define (path->table-row p) (vector (src-folder->friendly p)))
(define (get-folder idx) (vec-ref? (obs-peek @folders) idx))

(define (on-folder-select evt _entries idx)
  (when idx
    (case evt
      [(select) (@folder-selection . := . (get-folder idx))]
      [(dclick) (render (?dialog (path->string (get-folder idx))))])))

(define :folders-table
  (table '("Folders") @folders
         on-folder-select
         #:entry->row path->table-row))

;; ============================================================================
;; Components: Source documents table

(define (edit-source p)
  (thread
   (λ ()
     (define editor-path (obs-peek @editor))
     (define editor
       (if (and (string? editor-path) (not (string=? editor-path "")))
           editor-path
           "/usr/bin/open"))
     (define-values (sp out in err)
       (subprocess #f #f #f editor (path->string p)))
     (subprocess-wait sp)
     (log-msg "Opened: ~a" (file-name-from-path p))
     (close-input-port out)
     (close-output-port in)
     (close-input-port err))))

(define (get-source idx)
  (vector-ref (vec-ref? (obs-peek @sources-in-folder) idx) fullpath))

(define (update-sorting col)
  (define cur-sort (obs-peek @source-sorting))
  (cond
    [(= col (car cur-sort))
     (@source-sorting . := . (cons col (if (eq? (cdr cur-sort) string-ci>?)
                                            string-ci<?
                                            string-ci>?)))]
    [else
     (@source-sorting . := . (cons col (cdr cur-sort)))]))

(define (on-source-select evt _entries idx)
  (when idx
    (case evt
      [(column) (update-sorting idx)]
      [(select) (@source-doc-selection . := . (get-source idx))]
      [(dclick) (edit-source (get-source idx))])))

(define :source-docs-table
  (table '("Date" "Title" "Filename") @sources-in-folder
         on-source-select
         #:font mono-font
         #:column-widths '((0 100 100 100)
                           (1 300 100 600)
                           (2 200 100 300))))

;; ============================================================================
;; Components: Toolbar

(define button-size '(120 50))

(define (toolbar-button label action)
  (button label action
          #:min-size button-size
          #:style '(multi-line)))

(define (on-new-page-click)
  (render (?new-page)))

(define (on-start-preview-click)
  (match (obs-peek @stop-server-proc)
    [#f
     (define output (obs-peek @output-folder))
     (when output
       (define localhost:port (format "http://localhost:~a" localhost-port))
       (log-msg "Starting preview server (port ~a)" localhost-port)
       (@stop-server-proc . := . (start-server output #:port localhost-port #:watch? #t))
       (send-url localhost:port))]
    [_ (mindful-demure-server-stop)]))

(define (on-publish-click)
  (define site (obs-peek @site))
  (when site
    (define deploy-script (site-deploy-script site))
    (cond
      [(not deploy-script)
       (render (?dialog "No deploy script configured in site.rkt"))]
      [else
       (log-msg "Running deploy script: ~a" deploy-script)
       (thread
        (λ ()
          (define root (site-root site))
          (define output-folder (path->string (obs-peek @output-folder)))
          (define script-path (build-path root deploy-script))
          (define-values (sp out in err)
            (subprocess #f #f #f (path->string script-path) output-folder))
          (define result (port->string out))
          (close-input-port out)
          (close-output-port in)
          (close-input-port err)
          (subprocess-wait sp)
          (log-msg "Deploy complete")))])))

(define :toolbar
  (hpanel
   #:stretch '(#t #f)
   (toolbar-button "📄 New page" on-new-page-click)
   (toolbar-button @startstop-caption on-start-preview-click)
   (toolbar-button "🌐 Publish" on-publish-click)))

;; ============================================================================
;; Components: Menu

(define (on-add-site)
  (render (?add-site)))

(define (on-remove-site)
  (define current (obs-peek @site-selection))
  (when current
    (remove-from-pref! @sites current)
    (@site-selection . := . (match (obs-peek @sites)
                              ['() #f]
                              [(list* first _) first]))))

(define :main-menu
  (menu-bar
   (menu
    "File"
    (menu-item "Add site…" on-add-site)
    (menu-item "Remove this site" on-remove-site)
    (menu-item-separator)
    (menu-item "Preferences…" (λ () (render (?prefs)))))
   (menu
    "Help"
    (menu-item "About" (λ () (render (?dialog "Camp App v0.1")))))))

;; ============================================================================
;; Dialogs

(define (?new-page)
  (define-values (close! closing-mixin) (make-mix-close))
  (define/obs @title "")
  (define/obs @date (date->string (current-date) "~Y-~m-~d"))
  (define/obs @slug "")
  (define/obs @tags "")

  (define (create-page)
    (define site (obs-peek @site))
    (define folder (obs-peek @folder-selection))
    (when (and site folder)
      (define title (obs-peek @title))
      (define date (obs-peek @date))
      (define slug (obs-peek @slug))
      (define tags (obs-peek @tags))
      (define actual-slug
        (if (string=? slug "")
            (string-downcase (regexp-replace* #rx"[^a-zA-Z0-9]+" title "-"))
            slug))
      (define source-ext (site-sources site))
      (define filename (string-append actual-slug source-ext))
      (define filepath (build-path folder filename))

      (define content
        (string-append
         "#lang punct\n"
         "---\n"
         (format "title: ~a\n" title)
         (format "date: ~a\n" date)
         (if (string=? tags "") "" (format "tags: ~a\n" tags))
         "---\n\n"
         "Write your content here.\n"))

      (display-to-file content filepath #:exists 'error)
      (log-msg "Created: ~a" filename)
      (trigger-refresh!)
      (edit-source filepath))
    (close!))

  (dialog
   #:title "New page"
   #:mixin closing-mixin
   #:size '(400 #f)
   (vpanel
    (input @title (λ (_action s) (@title . := . s)) #:label "Title")
    (input @date (λ (_action s) (@date . := . s)) #:label "Date")
    (input @slug (λ (_action s) (@slug . := . s)) #:label "Slug (optional)")
    (input @tags (λ (_action s) (@tags . := . s)) #:label "Tags (optional)")
    (hpanel
     (button "Create" create-page)
     (button "Cancel" close!)))))

(define (?add-site)
  (define-values (close! closing-mixin) (make-mix-close))
  (define/obs @path "")

  (define (add-by-path)
    (define p (obs-peek @path))
    (unless (string=? p "")
      (cond
        [(file-exists? p)
         (add-to-pref! @sites p)
         (@site-selection . := . p)
         (close!)]
        [(directory-exists? p)
         (define site-file (build-path p "site.rkt"))
         (if (file-exists? site-file)
             (begin
               (add-to-pref! @sites (path->string site-file))
               (@site-selection . := . (path->string site-file))
               (close!))
             (render (?dialog "No site.rkt found in that folder")))]
        [else
         ;; Try as package name
         (define sym (string->symbol p))
         (define resolved (resolve-site-spec sym))
         (if resolved
             (begin
               (add-to-pref! @sites sym)
               (@site-selection . := . sym)
               (close!))
             (render (?dialog (format "Cannot find: ~a" p))))])))

  (define (browse-folder)
    (define path (get-directory "Select site folder"))
    (when path
      (@path . := . (path->string path))))

  (dialog
   #:title "Add site"
   #:mixin closing-mixin
   #:size '(500 #f)
   (vpanel
    (text "Enter a path to site.rkt, a folder containing site.rkt,")
    (text "or a package name:")
    (hpanel
     (input @path (λ (_action s) (@path . := . s)) #:min-size '(350 #f))
     (button "Browse…" browse-folder))
    (hpanel
     (button "Add" add-by-path)
     (button "Cancel" close!)))))

(define (?prefs)
  (define-values (close! closing-mixin) (make-mix-close))
  (dialog
   #:title "Preferences"
   #:mixin closing-mixin
   (vpanel
    (input @editor (λ (_action s) (@editor . := . s))
           #:label "Preferred editor (path)")
    (button "Close" close!))))

(define (current-date)
  (seconds->date (current-seconds)))

(define (date->string d fmt)
  (define yr (date-year d))
  (define mo (date-month d))
  (define dy (date-day d))
  (format "~a-~a-~a"
          yr
          (if (< mo 10) (format "0~a" mo) mo)
          (if (< dy 10) (format "0~a" dy) dy)))

;; ============================================================================
;; Main window

(define §app
  (window
   #:size '(900 700)
   #:stretch '(#t #t)
   #:title "Camp"
   #:mixin dragdrop-mix
   :main-menu
   (vpanel
    (hpanel
     (vpanel #:min-size '(180 #f)
             #:stretch '(#f #t)
             (button "➕ Add Site" on-add-site #:min-size '(150 40))
             :sites-choice
             :folders-table)
     (vpanel
      :toolbar
      :source-docs-table))
    (hpanel
     #:min-size '(#f 180)
     #:stretch '(#t #f)
     (log-output-textbox)))))

;; ============================================================================
;; Entry point

(define (run-app)
  (render §app))
