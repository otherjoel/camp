#lang racket/base

;; Minimal render functions for testing

(provide feed-content
         render-page
         render-post)

(define (feed-content doc)
  '(p "Feed content"))

(define (render-page doc context)
  `(html (body ,@(hash-ref context 'body))))

(define (render-post doc context)
  `(html (body ,@(hash-ref context 'body))))
