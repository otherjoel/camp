#lang racket/base

(require racket/file
         setup/dirs)

(provide installer)

(define app-name "Camp Computer.app")

(define (installer collects-parent own-dir user-specific?)
  (when (and user-specific? (eq? (system-type 'os) 'macosx))
    (define src (build-path (find-user-gui-bin-dir) app-name))
    (define dest-dir (build-path (find-system-path 'home-dir) "Applications"))
    (define dest (build-path dest-dir app-name))
    (when (directory-exists? src)
      (make-directory* dest-dir)
      (when (directory-exists? dest)
        (eprintf "Removing old: ~a\n" dest)
        (delete-directory/files dest))
      (eprintf "Copying app to: ~a\n" dest)
      (copy-directory/files src dest #:keep-modify-seconds? #t))))
