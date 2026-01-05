#lang racket/base

(require racket/string
         punct/core)

(provide defterm
         define-term
         term
         page-ref
         normalize-term-name
         normalize-slug
         content->text)

(define (normalize-term-name name)
  (define s (string-trim (string-downcase name)))
  (define no-ies (if (string-suffix? s "ies")
                     (string-append (substring s 0 (- (string-length s) 3)) "y")
                     s))
  (define no-sses (if (string-suffix? no-ies "sses")
                      (substring no-ies 0 (- (string-length no-ies) 2))
                      no-ies))
  (define no-s (if (and (string-suffix? no-sses "s")
                        (not (string-suffix? no-sses "ss")))
                   (substring no-sses 0 (- (string-length no-sses) 1))
                   no-sses))
  (string-replace (regexp-replace* #rx"[ \t]+" no-s " ") " " "-"))

(define (normalize-slug s)
  (string-downcase (string-normalize-spaces (string-trim s) #px"[^A-Za-z0-9]+" "-")))

(define (content->text content)
  (apply string-append
         (for/list ([elem (in-list content)])
           (cond
             [(string? elem) elem]
             [(list? elem)
              (define children
                (if (and (pair? (cdr elem))
                         (list? (cadr elem))
                         (or (null? (cadr elem))
                             (and (pair? (cadr elem)) (list? (car (cadr elem))))))
                    (cddr elem)
                    (cdr elem)))
              (content->text children)]
             [else ""]))))

(define (defterm . content)
  (define term-text (content->text content))
  (cons-to-metas-list 'terms-defined term-text)
  `(term-definition ,@content))

(define define-term defterm)

(define (term . content)
  `(term ,@content))

(define (page-ref slug-or-text . content)
  (define slug (normalize-slug slug-or-text))
  `(page-ref ((slug ,slug)) ,@content))