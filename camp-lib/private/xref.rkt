#lang racket/base

(require racket/string
         punct/core)

(provide defterm
         define-term  ; alias for defterm
         term
         page-ref
         normalize-term-name
         content->text)

;; Normalize a term name for use in HTML ids and URL fragments.
;; Follows Scribble's normalization: lowercase, ies→y, sses→ss, trailing s removal,
;; and whitespace collapsing. This allows "APIs" to match "api", "libraries" to match "library".
(define (normalize-term-name name)
  (define s (string-trim (string-downcase name)))
  ;; Handle "ies" → "y" (e.g., "libraries" → "library")
  (define no-ies (if (string-suffix? s "ies")
                     (string-append (substring s 0 (- (string-length s) 3)) "y")
                     s))
  ;; Handle "sses" → "ss" (e.g., "classes" → "class")
  (define no-sses (if (string-suffix? no-ies "sses")
                      (substring no-ies 0 (- (string-length no-ies) 2))
                      no-ies))
  ;; Handle trailing "s" but preserve "ss" (e.g., "APIs" → "api", "class" → "class")
  (define no-s (if (and (string-suffix? no-sses "s")
                        (not (string-suffix? no-sses "ss")))
                   (substring no-sses 0 (- (string-length no-sses) 1))
                   no-sses))
  ;; Collapse whitespace and replace with hyphens
  (string-replace (regexp-replace* #rx"[ \t]+" no-s " ") " " "-"))

;; Extract plain text from content (which may contain nested xexprs).
;; Used to derive the term key from content for normalization.
;; Handles xexprs in both forms: (tag ((attr val)...) child...) and (tag child...)
(define (content->text content)
  (apply string-append
         (for/list ([elem (in-list content)])
           (cond
             [(string? elem) elem]
             [(list? elem)
              ;; xexpr can be (tag attrs child...) or (tag child...) without attrs
              ;; attrs is a list of lists like ((attr val)...), so check if second element
              ;; is a list of lists (attrs) or something else (first child)
              (define children
                (if (and (pair? (cdr elem))
                         (list? (cadr elem))
                         (or (null? (cadr elem))
                             (and (pair? (cadr elem)) (list? (car (cadr elem))))))
                    (cddr elem)  ; has attrs, skip tag and attrs
                    (cdr elem))) ; no attrs, skip just tag
              (content->text children)]
             [else ""]))))

;; Define a term. The content is displayed as-is; the normalized key is derived from it.
;; Like Scribble's deftech: •define-term{pianoforte} displays "pianoforte" and creates
;; an anchor with id="term-pianoforte".
(define (defterm . content)
  (define term-text (content->text content))
  (cons-to-metas-list 'terms-defined term-text)
  `(term-definition () ,@content))

;; Alias for defterm
(define define-term defterm)

;; Reference a term. The content is displayed as-is; the normalized key is used for lookup.
;; Like Scribble's tech: •term{APIs} displays "APIs" but looks up "api".
(define (term . content)
  `(term () ,@content))

(define (page-ref slug-or-text . content)
  (define slug (string-replace slug-or-text " " "-"))
  (if (null? content)
      `(page-ref ((slug ,slug)))
      `(page-ref ((slug ,slug)) ,@content)))
