#lang racket/base

;; Camp App - GUI application for managing Camp static sites
;; Run with: racket -l camp/app

(require camp/app/private/app)

(module+ main
  (run-app))
