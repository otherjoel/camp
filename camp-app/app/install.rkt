#lang racket/base

(require racket/file
         setup/dirs
         launcher/launcher)

(provide installer launcher-flags)

(define app-name "Camp Computer")

;; The launcher raco setup builds embeds only the collects and config dirs, so it
;; fails to launch whenever the addon dir or compiled-file roots differ from
;; Racket's compiled-in defaults (e.g. under per-toolchain managers like rackup,
;; which relocate both via PLTADDONDIR/PLTCOMPILEDROOTS). Pin all three with
;; -A/-G/-R at build time; under a stock Racket these are the defaults anyway.
(define (launcher-flags #:addon-dir addon-dir
                        #:config-dir config-dir
                        #:compiled-roots roots)
  (append (list "-A" (path->string addon-dir)
                "-G" (path->string config-dir))
          (if roots (list "-R" roots) null)
          (list "-l-" "camp/app.rkt")))

(define (installer collects-parent own-dir user-specific?)
  (when (and user-specific? (eq? (system-type 'os) 'macosx))
    (define dest-dir (build-path (find-system-path 'home-dir) "Applications"))
    (define dest (build-path dest-dir (string-append app-name ".app")))
    (make-directory* dest-dir)
    (when (directory-exists? dest)
      (delete-directory/files dest))
    (eprintf "Building app: ~a\n" dest)
    (make-gracket-launcher
     (launcher-flags #:addon-dir (find-system-path 'addon-dir)
                     #:config-dir (find-config-dir)
                     #:compiled-roots (getenv "PLTCOMPILEDROOTS"))
     dest
     (cons `(exe-name . ,app-name)
           (build-aux-from-path (build-path own-dir "app")))
     #:tether-mode #f)))
