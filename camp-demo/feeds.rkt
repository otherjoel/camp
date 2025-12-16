#lang racket/base

(require punct/doc)

(provide feed-content)

;; Render function for feed entries
;; Returns the HTML content for an entry's body
(define (feed-content doc)
  ;; For feeds, we render a simplified version without cross-ref resolution
  ;; (URLs in feeds should be absolute anyway)
  `(article ,@(document-body doc)))