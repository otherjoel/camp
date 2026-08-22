#lang info
(define collection "camp")
(define deps '("net-lib"
               "punct-lib"
               "base"
               "camp-lib"
               "drracket-vim-tool"
               "gregor-lib"
               "gui-easy-lib"
               "gui-lib"))
(define implies '("camp-lib"))
(define pkg-desc "GUI application for Camp static site generator")
(define version "1.0")
(define pkg-authors '("Joel Dueck"))
(define license 'LicenseRef-CreatorCxn-1.0)
(define build-deps '())
(define gracket-launcher-names '("Camp Computer"))
(define gracket-launcher-libraries '("app.rkt"))
(define install-collection "app/install.rkt")