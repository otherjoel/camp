#lang info
(define collection "camp")
(define deps '("base"
               "gregor-lib"
               "hash-view-lib"
               "html-printer"
               "punct-lib"
               "splitflap-lib"
               "toml-config-lib"
               "web-server-lib"))
(define pkg-desc "Implementation part of Camp")
(define version "0.0")
(define pkg-authors '("Joel Dueck"))
(define license '(Apache-2.0 OR MIT))
(define build-deps '())

;; Register raco camp command
(define raco-commands '(("camp" camp/private/cli "Camp static site generator" #f)))
