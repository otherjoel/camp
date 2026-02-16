#lang info
(define collection "camp")
(define deps '("base" "camp-lib"))
(define implies '("camp-lib"))
(define scribblings '(("scribblings/camp.scrbl" (multi-page))))
(define compile-omit-paths '("scribblings/mysite"))
(define pkg-desc "Docs for Camp")
(define version "1.0")
(define pkg-authors '("Joel Dueck"))
(define license 'LicenseRef-CreatorCxn-1.0)
(define build-deps '("debug"
                     "gregor-doc"
                     "gregor-lib"
                     "hash-view"
                     "punct-doc"
                     "punct-lib"
                     "splitflap-doc"
                     "splitflap-lib"
                     "racket-doc"
                     "rackunit-lib"
                     "scribble-lib"))
