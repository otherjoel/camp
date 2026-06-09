#lang racket/base

;; Tests for load-doc: page modules must reload after their source changes,
;; even within a single long-running process (see camp-app live rebuilds).

(require rackunit
         racket/file
         punct/fetch
         (only-in camp/private/collections load-doc))

(define (write-page! p body)
  (display-to-file
   (format "#lang punct\n---\ntitle: Test Page\ndate: 2026-01-01\n---\n\n~a\n" body)
   p #:exists 'replace))

;; Editing and rebuilding in quick succession can land within the same
;; mtime second, which rerequire would not detect; force the clock forward.
(define (touch-later! p)
  (file-or-directory-modify-seconds
   p (+ (file-or-directory-modify-seconds p) 2)))

(define (doc-body-string p)
  (format "~a" (load-doc p)))

;; ---------------------------------------------------------------------------
;; load-doc picks up source edits across repeated loads

(define page-a (make-temporary-file "load-doc-test-a-~a.md.rkt"))
(write-page! page-a "VERSION ONE")
(check-regexp-match #rx"VERSION ONE" (doc-body-string page-a))

(write-page! page-a "VERSION TWO")
(touch-later! page-a)
(check-regexp-match #rx"VERSION TWO" (doc-body-string page-a))
(delete-file page-a)

;; ---------------------------------------------------------------------------
;; A plain get-doc AFTER a load-doc must not pin the module to a stale
;; version. (The reverse order would: rerequire never reloads a module it
;; did not load first, which is why all camp doc loading goes via load-doc.)

(define page-b (make-temporary-file "load-doc-test-b-~a.md.rkt"))
(write-page! page-b "FIRST DRAFT")
(check-regexp-match #rx"FIRST DRAFT" (doc-body-string page-b))
(check-regexp-match #rx"FIRST DRAFT" (format "~a" (get-doc page-b)))

(write-page! page-b "SECOND DRAFT")
(touch-later! page-b)
(check-regexp-match #rx"SECOND DRAFT" (doc-body-string page-b))
(check-regexp-match #rx"SECOND DRAFT" (format "~a" (get-doc page-b)))
(delete-file page-b)
