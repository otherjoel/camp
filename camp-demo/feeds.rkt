#lang racket/base

(require punct/doc
         html-printer)

(provide feed-content)

;; Render function for feed entries
;; Returns the HTML content for an entry's body
(define (feed-content doc)
  ;; For feeds, we render a simplified version without cross-ref resolution
  ;; (URLs in feeds should be absolute anyway)
  (define body-elements (doc-body doc))

  ;; Convert to HTML string using html-printer
  (apply string-append
         (map (λ (x) (xexpr->html5 x)) body-elements)))
