#lang racket/base

;; Camp App - Main application window and logic

(require camp
         camp/build
         camp/serve
         (only-in camp/private/build sync-static-files output-path->url)
         (only-in camp/private/xref normalize-slug)
         (only-in camp/private/watch start-watcher! get-watch-paths path-change-type)
         (only-in camp/private/output format-duration with-timing)
         (only-in gregor iso8601->date)
         net/sendurl
         racket/exn
         racket/file
         racket/gui
         racket/gui/easy
         racket/gui/easy/operator
         racket/list
         racket/match
         racket/path
         racket/port
         racket/rerequire
         racket/runtime-path
         racket/string
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
;; Toolbar icons

(define-runtime-path icon-new-page "icons/new-page.png")
(define-runtime-path icon-build    "icons/build.png")
(define-runtime-path icon-start    "icons/start.png")
(define-runtime-path icon-stop     "icons/stop.png")
(define-runtime-path icon-publish  "icons/publish.png")

(define icon-size 32)

(define (try-read-bitmap path)
  (and (file-exists? path)
       (let* ([src (read-bitmap path)]
              [w (send src get-width)]
              [h (send src get-height)]
              [bs (get-display-backing-scale)]
              [dest (make-bitmap icon-size icon-size #:backing-scale bs)]
              [dc (send dest make-dc)])
         (send dc set-smoothing 'smoothed)
         (send dc draw-bitmap-section-smooth src 0 0 icon-size icon-size 0 0 w h)
         dest)))

(define bmp-new-page (try-read-bitmap icon-new-page))
(define bmp-build    (try-read-bitmap icon-build))
(define bmp-start    (try-read-bitmap icon-start))
(define bmp-stop     (try-read-bitmap icon-stop))
(define bmp-publish  (try-read-bitmap icon-publish))

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
                       (log-msg "Build error:\n~a" (exn->string e)))])
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

(define month-names #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                      "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(define (format-display-date date-val)
  (define date-str (~a date-val))
  (cond
    [(regexp-match #px"^(\\d{4})-(\\d{2})-(\\d{2})" date-str)
     => (λ (m)
          (define month (string->number (list-ref m 2)))
          (define day (string->number (list-ref m 3)))
          (format "~a ~a, ~a" (vector-ref month-names (sub1 month)) day (list-ref m 1)))]
    [else date-str]))

(define (source-entry->row entry)
  (vector (format-display-date (vector-ref entry date-col))
          (~a (vector-ref entry title-col))
          (~a (vector-ref entry file-col))))

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

(define (rerequire-render-modules! site)
  (for ([coll (in-list (site-collections site))])
    (define spec (collection-render-with coll))
    (when spec
      (with-handlers ([exn:fail? void])
        (dynamic-rerequire (car spec)))))
  (for ([feed (in-list (site-feeds site))])
    (with-handlers ([exn:fail? void])
      (dynamic-rerequire (car (feed-config-render-with feed)))))
  (when (site-default-render site)
    (with-handlers ([exn:fail? void])
      (dynamic-rerequire (car (site-default-render site))))))

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
     (define file-arg (path->string p))
     (define-values (sp out in err)
       (cond
         [(and (non-empty-string? editor-path)
               (eq? (system-type 'os) 'macosx)
               (regexp-match? #rx"\\.app$" editor-path))
          (subprocess #f #f #f "/usr/bin/open" "-a" editor-path file-arg)]
         [(non-empty-string? editor-path)
          (subprocess #f #f #f editor-path file-arg)]
         [(eq? (system-type 'os) 'macosx)
          (subprocess #f #f #f "/usr/bin/open" file-arg)]
         [else
          (subprocess #f #f #f (find-executable-path "xdg-open") file-arg)]))
     (subprocess-wait sp)
     (log-msg "Opened: ~a" (file-name-from-path p))
     (close-input-port out)
     (close-output-port in)
     (close-input-port err))))

(define (get-source idx)
  (vector-ref (vec-ref? (obs-peek @sources-in-folder) idx) fullpath))

(define (get-source-url idx)
  (vector-ref (vec-ref? (obs-peek @sources-in-folder) idx) file-col))

(define (delete-source p)
  (define name (path->string (file-name-from-path p)))
  (define confirm
    (message-box "Delete page"
                 (format "Delete ~a?" name)
                 (get-main-frame) '(yes-no caution)))
  (when (eq? confirm 'yes)
    (delete-file p)
    (log-msg "Deleted: ~a" name)
    (trigger-refresh!)))

(define (update-sorting col)
  (define cur-sort (obs-peek @source-sorting))
  (cond
    [(= col (car cur-sort))
     (@source-sorting . := . (cons col (if (eq? (cdr cur-sort) string-ci>?)
                                            string-ci<?
                                            string-ci>?)))]
    [else
     (@source-sorting . := . (cons col (cdr cur-sort)))]))

(define (show-source-context-menu list-box x y)
  (define idx (send list-box get-selection))
  (when idx
    (define p (get-source idx))
    (define page-url (get-source-url idx))
    (define menu (new popup-menu%))
    (new menu-item% [parent menu] [label "Edit"]
         [callback (λ (_item _evt) (edit-source p))])
    (new menu-item% [parent menu] [label "Preview"]
         [callback (λ (_item _evt)
                     (when (ensure-server-running!)
                       (send-url (format "http://localhost:~a~a" localhost-port page-url))))])
    (new separator-menu-item% [parent menu])
    (new menu-item% [parent menu] [label "Delete"]
         [callback (λ (_item _evt) (delete-source p))])
    (send list-box popup-menu menu x y)))

(define (on-source-select evt _entries idx)
  (when idx
    (case evt
      [(column) (update-sorting idx)]
      [(select) (@source-doc-selection . := . (get-source idx))]
      [(dclick) (edit-source (get-source idx))])))

(define source-table-mixin
  (λ (%)
    (class %
      (super-new)
      (define/override (on-subwindow-event receiver event)
        (cond
          [(and (eq? receiver this)
                (send event button-down? 'right))
           (show-source-context-menu this
                                     (send event get-x)
                                     (send event get-y))]
          [else (super on-subwindow-event receiver event)])))))

(define :source-docs-table
  (table '("Date" "Title" "URL") @sources-in-folder
         on-source-select
         #:entry->row source-entry->row
         #:mixin source-table-mixin
         #:column-widths '((0 120 80 150)
                           (1 300 100 600)
                           (2 200 100 300))))

;; ============================================================================
;; Components: Toolbar

(define button-size '(120 50))

(define (toolbar-button label action #:icon [icon #f])
  (button (if icon (list icon label 'top) label)
          action
          #:min-size button-size
          #:style '(multi-line)))

(define (on-new-page-click)
  (render (?new-page)))

(define (ensure-server-running!)
  (cond
    [(obs-peek @stop-server-proc) #t]
    [else
     (define site (obs-peek @site))
     (define output (obs-peek @output-folder))
     (cond
       [(not (and site output)) #f]
       [else
        (log-msg "Starting preview server (port ~a)" localhost-port)
        (define shutdown-server (start-server output #:port localhost-port #:watch? #t))

        ;; File watching
        (define site-config-path
          (simplify-path (path->complete-path (resolve-site-spec (obs-peek @site-selection)))))
        (define manifest-path (make-temporary-file "camp-static-~a"))

        (define (handle-change changed-path)
          (define rel (find-relative-path (site-root site) changed-path))
          (define rel-str (if (equal? rel changed-path)
                              (path->string (file-name-from-path changed-path))
                              (path->string rel)))
          (define change-type (path-change-type changed-path site site-config-path))
          (log-msg "~a changed" rel-str)
          (define-values (result rebuild-ms)
            (with-timing
              (case change-type
                [(static)
                 (define static-dir (build-path (site-root site) (site-static-folder site)))
                 (with-handlers ([exn:fail? (λ (e) (log-msg "Static sync error:\n~a" (exn->string e)) #f)])
                   (sync-static-files static-dir output manifest-path)
                   #t)]
                [(rkt)
                 (rerequire-render-modules! site)
                 (build-site!)
                 #t]
                [else (build-site!) #t])))
          (when result
            (log-msg "Done (~a)" (format-duration rebuild-ms))))

        (define watch-paths (get-watch-paths site site-config-path))
        (define stop-watcher (start-watcher! watch-paths handle-change))

        (@stop-server-proc . := .
         (λ ()
           (stop-watcher)
           (shutdown-server)
           (when (file-exists? manifest-path)
             (delete-file manifest-path))))
        #t])]))

(define (on-start-preview-click)
  (match (obs-peek @stop-server-proc)
    [#f (when (ensure-server-running!)
          (send-url (format "http://localhost:~a" localhost-port)))]
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
            (parameterize ([current-directory root])
              (subprocess #f #f 'stdout (path->string script-path) output-folder)))
          (close-output-port in)
          (for ([line (in-lines out)])
            (log-msg "  ~a" line))
          (close-input-port out)
          (subprocess-wait sp)
          (define exit-code (subprocess-status sp))
          (if (zero? exit-code)
              (log-msg "Deploy complete")
              (log-msg "Deploy failed (exit code ~a)" exit-code))))])))

(define (on-build-click)
  (thread build-site!))

(define :startstop-button
  (button (if bmp-start (list bmp-start "Start preview" 'top) "Start preview")
          on-start-preview-click
          #:min-size button-size
          #:style '(multi-line)
          #:mixin (λ (%)
                    (class %
                      (super-new)
                      (obs-observe!
                       @stop-server-proc
                       (λ (v)
                         (queue-callback
                          (λ ()
                            (send this set-label (if v "Stop preview" "Start preview"))
                            (define bmp (if v bmp-stop bmp-start))
                            (when bmp (send this set-label bmp))))))))))

(define :toolbar
  (hpanel
   #:stretch '(#t #f)
   (toolbar-button "New page" on-new-page-click #:icon bmp-new-page)
   (toolbar-button "Build site" on-build-click #:icon bmp-build)
   :startstop-button
   (button (if bmp-publish (list bmp-publish "Publish" 'top) "Publish")
          on-publish-click
          #:enabled? (obs-map @site (λ (s) (and s (site-deploy-script s) #t)))
          #:min-size button-size
          #:style '(multi-line))))

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

  ;; Determine taxonomies and output pattern for the current collection
  (define site (obs-peek @site))
  (define folder (obs-peek @folder-selection))
  (define coll (and site folder (folder->collection site folder)))
  (define taxonomy-names (if coll (collection-taxonomies coll) '()))
  (define taxonomy-obs
    (for/list ([name (in-list taxonomy-names)])
      (cons name (@ ""))))
  (define output-pattern (and coll (collection-output-paths coll)))

  (define @url-preview
    (obs-combine
     (λ (title date slug)
       (define actual-slug (normalize-slug (if (non-empty-string? slug) slug title)))
       (if (and output-pattern (non-empty-string? actual-slug))
           (with-handlers ([exn:fail? (λ (_) "")])
             (define date-val
               (and (non-empty-string? date) (iso8601->date date)))
             (output-path->url (format-output-path output-pattern actual-slug date-val)))
           ""))
     @title @date @slug))

  (define (create-page)
    (when (and site folder)
      (define title (obs-peek @title))
      (define date (obs-peek @date))
      (define slug (obs-peek @slug))
      (define source-ext (site-sources site))
      (define filename (string-append (normalize-slug title) source-ext))
      (define filepath (build-path folder filename))

      (define taxonomy-lines
        (for/list ([pair (in-list taxonomy-obs)]
                   #:unless (string=? (obs-peek (cdr pair)) ""))
          (format "~a: ~a\n" (car pair) (obs-peek (cdr pair)))))

      (define content
        (string-append
         "#lang punct\n"
         "---\n"
         (format "title: ~a\n" title)
         (format "date: ~a\n" date)
         (if (non-empty-string? slug) (format "slug: ~a\n" slug) "")
         (apply string-append taxonomy-lines)
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
   (apply vpanel
     (append
       (list
         (input @title (λ (_action s) (@title . := . s)) #:label "Title")
         (input @date (λ (_action s) (@date . := . s)) #:label "Date")
         (input @slug (λ (_action s) (@slug . := . s)) #:label "Slug (optional)")
         (text @url-preview))
       (for/list ([pair (in-list taxonomy-obs)])
         (input (cdr pair)
                (λ (_action s) ((cdr pair) . := . s))
                #:label (format "~a (optional)" (string-titlecase (car pair)))))
       (list
         (hpanel
           (button "Create" create-page)
           (button "Cancel" close!)))))))

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
    (define path (get-directory "Select site folder" (get-main-frame)))
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

(define (editor-display-name path)
  (cond
    [(or (not path) (and (string? path) (string=? path ""))) "System default"]
    [(path? path) (path->string (file-name-from-path path))]
    [else (let ([p (string->path path)]) (path->string (file-name-from-path p)))]))

(define (choose-editor!)
  (define default-dir
    (and (eq? (system-type 'os) 'macosx)
         (string->path "/Applications/")))
  (define path (get-file "Choose editor" (get-main-frame) default-dir))
  (when path
    (@editor . := . (path->string path))))

(define (?prefs)
  (define-values (close! closing-mixin) (make-mix-close))
  (define @editor-label (obs-map @editor editor-display-name))
  (dialog
   #:title "Preferences"
   #:mixin closing-mixin
   (vpanel
    (hpanel
     (text @editor-label)
     (button "Choose editor…" (λ () (choose-editor!)))
     (button "Clear" (λ () (@editor . := . ""))))
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
   #:mixin (λ (%) (dragdrop-mix (class % (super-new) (set-main-frame! this))))
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
