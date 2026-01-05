#lang racket/base

;; Minimal render functions for testing

(require camp)

(provide feed-content
         render-page
         render-post)

(define (feed-content doc context)
  `(p "Feed content for " ,(context-url context)))

(define (render-page doc context)
  `(html (body ,@(camp-doc->html-xexpr doc))))

(define (render-post doc context)
  `(html (body ,@(camp-doc->html-xexpr doc))))
