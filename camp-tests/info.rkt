#lang info
(define collection "camp")
(define deps '("base"
               "camp-lib"
               "gregor-lib"
               "rackunit-lib"))
(define build-deps '())
(define pkg-desc "Tests for Camp")
(define version "0.0")
(define pkg-authors '("Joel Dueck"))
(define license '(Apache-2.0 OR MIT))

;; Exclude fixtures from compilation and test discovery
(define compile-omit-paths '("tests/fixtures"))
(define test-omit-paths '("tests/fixtures"))
