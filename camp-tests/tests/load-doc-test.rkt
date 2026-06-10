#lang racket/base

;; Tests for load-doc and live-reload bytecode handling: page modules must
;; reload after their source changes within a single long-running process
;; (see camp-app live rebuilds), while one-shot builds keep using compiled
;; bytecode.

(require rackunit
         compiler/cm
         compiler/compilation-path
         racket/file
         racket/system
         setup/dirs
         punct/fetch
         (only-in camp/private/collections load-doc)
         (only-in camp/private/rerequire live-reload? clear-site-bytecode!))

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
;; Outside live-reload mode (one-shot builds), loading must not discard
;; compiled bytecode — that is the compile cache for raco camp build.

(define keep-dir (make-temporary-directory "load-doc-test-keep-~a"))
(define keep-page (build-path keep-dir "page.md.rkt"))
(write-page! keep-page "CACHED PAGE")
(managed-compile-zo keep-page)
(define keep-zo (get-compilation-bytecode-file keep-page))
(check-true (file-exists? keep-zo))
(check-regexp-match #rx"CACHED PAGE" (doc-body-string keep-page))
(check-true (file-exists? keep-zo) "one-shot loads must keep compiled bytecode")
(delete-directory/files keep-dir)

;; Everything below exercises live-reload mode (GUI app, raco camp serve)
(live-reload? #t)

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

;; ---------------------------------------------------------------------------
;; Editing a module a page depends on must reload the page, even when the
;; page source itself is untouched. (Site authors edit root main.rkt to add
;; functions for use in page sources; rerequire walks recorded dependencies.)

(define dep-dir (make-temporary-directory "load-doc-test-deps-~a"))
(define helper (build-path dep-dir "helper.rkt"))
(define dep-page (build-path dep-dir "dep-page.md.rkt"))

(define (write-helper! h greeting)
  (display-to-file
   (format "#lang racket/base\n(provide greeting)\n(define (greeting) ~s)\n" greeting)
   h #:exists 'replace))

(write-helper! helper "HELLO FROM V1")
(display-to-file
 "#lang punct \"helper.rkt\"\n---\ntitle: Dep Page\ndate: 2026-01-01\n---\n\n•greeting[]\n"
 dep-page #:exists 'replace)

(check-regexp-match #rx"HELLO FROM V1" (doc-body-string dep-page))

(write-helper! helper "HELLO FROM V2")
(touch-later! helper)
(check-regexp-match #rx"HELLO FROM V2" (doc-body-string dep-page))
(delete-directory/files dep-dir)

;; ---------------------------------------------------------------------------
;; A page whose first load would come from raco-compiled bytecode must still
;; be reloadable: bytecode is compiled with constant enforcement, so a module
;; first declared from it can never be redeclared ("cannot re-define a
;; constant"). In live-reload mode, load-doc removes the bytecode and
;; declares from source.

(define zo-dir (make-temporary-directory "load-doc-test-zo-~a"))
(define page-c (build-path zo-dir "compiled-page.md.rkt"))
(write-page! page-c "COMPILED ONE")
(managed-compile-zo page-c)

(check-regexp-match #rx"COMPILED ONE" (doc-body-string page-c))

(write-page! page-c "COMPILED TWO")
(touch-later! page-c)
(check-regexp-match #rx"COMPILED TWO" (doc-body-string page-c))
(delete-directory/files zo-dir)

;; ---------------------------------------------------------------------------
;; Same constraint, one level down: a raco-compiled DEPENDENCY of a page
;; (e.g. the site's main.rkt) is loaded by rerequire's own handler, which
;; load-doc cannot intercept per-module. clear-site-bytecode! sweeps the
;; site tree at session start so those first declarations also come from
;; source. (load-site performs this sweep when live-reload? is on.)

(define cdep-dir (make-temporary-directory "load-doc-test-cdep-~a"))
(define chelper (build-path cdep-dir "helper.rkt"))
(define cdep-page (build-path cdep-dir "dep-page.md.rkt"))

(write-helper! chelper "COMPILED DEP V1")
(display-to-file
 "#lang punct \"helper.rkt\"\n---\ntitle: Dep Page\ndate: 2026-01-01\n---\n\n•greeting[]\n"
 cdep-page #:exists 'replace)
;; Compile in a subprocess, as raco setup would: compiling in this process
;; would load the helper's declaration here and defeat the point of the test
(check-true (system* (build-path (find-console-bin-dir) "raco")
                     "make" (path->string cdep-page)))

(clear-site-bytecode! cdep-dir)
(check-false (file-exists? (get-compilation-bytecode-file chelper))
             "clear-site-bytecode! removes compiled bytecode in the tree")

(check-regexp-match #rx"COMPILED DEP V1" (doc-body-string cdep-page))

(write-helper! chelper "COMPILED DEP V2")
(touch-later! chelper)
(check-regexp-match #rx"COMPILED DEP V2" (doc-body-string cdep-page))
(delete-directory/files cdep-dir)

(live-reload? #f)
