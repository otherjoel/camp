#lang info
(define collection "camp")
(define deps '("net-lib"
               "punct-lib"
               "base"
               "camp-lib"
               "gui-easy-lib"
               "gui-lib"))
(define pkg-desc "GUI application for Camp static site generator")
(define version "0.0")
(define pkg-authors '("Joel Dueck"))
(define license '(Apache-2.0 OR MIT))
(define build-deps '())
(define gracket-launcher-names '("Camp Computer.app"))
(define gracket-launcher-libraries '("app.rkt"))
(define install-collection "app/install.rkt")