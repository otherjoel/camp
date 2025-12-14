#lang racket/base

(require racket/logging)

(provide camp-logger
         log-camp-fatal
         log-camp-error
         log-camp-warning
         log-camp-info
         log-camp-debug)

(define-logger camp)
