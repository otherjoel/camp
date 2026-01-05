#lang racket/base

(require camp
         punct/doc)

(provide feed-content)

;; Render function for feed entries
;; Returns the HTML content for an entry's body
(define (feed-content doc context)
  `(article
    ,@(document-body doc)
    (p (a ((href ,(context-url context))) "Read more →"))))