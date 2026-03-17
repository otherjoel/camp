#lang info
(define collection "camp")
(define deps '("base"
               "camp-lib"
               "gregor-lib"
               "rackunit-lib"))
(define build-deps '("punct-lib"
                     "splitflap-lib"
                     "toml-config-lib"
                     ))
(define pkg-desc "Tests for Camp")
(define version "1.0")
(define pkg-authors '("Joel Dueck"))
(define license 'LicenseRef-CreatorCxn-1.0)

;; Exclude fixtures from test discovery (but not compilation, since render.rkt is required)
(define test-omit-paths '("tests/fixtures"))
