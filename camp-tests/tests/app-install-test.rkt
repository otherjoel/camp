#lang racket/base

;; Tests for the GUI app installer's launcher flag computation

(require rackunit
         rackunit/text-ui
         camp/app/install)

(define app-install-tests
  (test-suite
   "App installer launcher flags"

   (test-case "pins addon and config dirs, names the app module"
     (check-equal? (launcher-flags #:addon-dir (string->path "/addons/tc")
                                   #:config-dir (string->path "/install/etc")
                                   #:compiled-roots #f)
                   '("-A" "/addons/tc" "-G" "/install/etc" "-l-" "camp/app.rkt")))

   (test-case "includes -R only when compiled roots are set"
     (check-equal? (launcher-flags #:addon-dir (string->path "/addons/tc")
                                   #:config-dir (string->path "/install/etc")
                                   #:compiled-roots "compiled/9.2-cs:.")
                   '("-A" "/addons/tc" "-G" "/install/etc"
                     "-R" "compiled/9.2-cs:." "-l-" "camp/app.rkt")))))

(module+ main
  (run-tests app-install-tests))

(module+ test
  (run-tests app-install-tests))
