#lang racket/base

;; Tests for load-doc and live-reload bytecode handling: page modules must
;; reload after their source changes within a single long-running process
;; (see camp-app live rebuilds), while one-shot builds keep using compiled
;; bytecode. Live sessions cache bytecode in a private "camp-live" mode dir
;; so later sessions skip recompiling unchanged sources; raco-built bytecode
;; is never loaded (it forbids redeclaration) but also never deleted.

(require rackunit
         compiler/cm
         compiler/compilation-path
         racket/file
         racket/list
         racket/path
         racket/system
         setup/dirs
         punct/fetch
         (only-in camp/private/collections load-doc)
         (only-in camp/private/rerequire live-reload? live-cache-root!))

;; Edits within the same real-time second would evade rerequire's mtime
;; checks, while future-dated sources are refused by the compilation manager.
;; A virtual clock gives every write a distinct mtime safely in the past.
(define virtual-now (- (current-seconds) 1000))
(define (touch-next! p)
  (set! virtual-now (add1 virtual-now))
  (file-or-directory-modify-seconds p virtual-now))

(define (write-mod! p content)
  (display-to-file content p #:exists 'replace)
  (touch-next! p))

(define (write-page! p body)
  (write-mod! p (format "#lang punct\n---\ntitle: Test Page\ndate: 2026-01-01\n---\n\n~a\n" body)))

(define (write-helper! h greeting)
  (write-mod! h (format "#lang racket/base\n(provide greeting)\n(define (greeting) ~s)\n" greeting)))

(define (write-dep-page! p body)
  (write-mod! p (format "#lang punct \"helper.rkt\"\n---\ntitle: Dep Page\ndate: 2026-01-01\n---\n\n~a\n" body)))

(define (doc-body-string p)
  (format "~a" (load-doc p)))

;; The live cache for src lives in a "camp-live" mode dir next to it,
;; possibly rerooted by current-compiled-file-roots.
(define (live-zo-exists? src)
  (define-values (dir name _dir?) (split-path src))
  (define zo-name (path-add-extension name #".zo" #"_"))
  (for/or ([p (in-directory dir)])
    (and (equal? (file-name-from-path p) zo-name)
         (member (string->path "camp-live") (explode-path p))
         #t)))

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
(check-false (live-zo-exists? keep-page) "one-shot loads must not write the live cache")
(delete-directory/files keep-dir)

;; Everything below exercises live-reload mode (GUI app, raco camp serve)
(live-reload? #t)

;; ---------------------------------------------------------------------------
;; load-doc picks up source edits across repeated loads

(define page-a (make-temporary-file "load-doc-test-a-~a.md.rkt"))
(write-page! page-a "VERSION ONE")
(check-regexp-match #rx"VERSION ONE" (doc-body-string page-a))

(write-page! page-a "VERSION TWO")
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

(write-helper! helper "HELLO FROM V1")
(write-dep-page! dep-page "•greeting[]")

(live-cache-root! dep-dir)
(check-regexp-match #rx"HELLO FROM V1" (doc-body-string dep-page))

(write-helper! helper "HELLO FROM V2")
(check-regexp-match #rx"HELLO FROM V2" (doc-body-string dep-page))

;; Compile-time staleness: a dependency edit that adds an export, used by an
;; edited page, must not expand the page against a stale declaration of the
;; dependency lingering in the compilation namespace from earlier builds.
(write-mod! helper
            (string-append "#lang racket/base\n(provide greeting shout)\n"
                           "(define (greeting) \"HELLO FROM V3\")\n"
                           "(define (shout) \"NEW EXPORT\")\n"))
(write-dep-page! dep-page "•greeting[] •shout[]")
(check-regexp-match #rx"HELLO FROM V3.*NEW EXPORT" (doc-body-string dep-page))
(delete-directory/files dep-dir)

;; ---------------------------------------------------------------------------
;; A page with raco-compiled bytecode must still be reloadable: that bytecode
;; is compiled with constant enforcement, so a module first declared from it
;; can never be redeclared ("cannot re-define a constant"). Live loads must
;; ignore it (loading via the camp-live cache instead) — but not delete it,
;; since it is the CLI's compile cache.

(define zo-dir (make-temporary-directory "load-doc-test-zo-~a"))
(define page-c (build-path zo-dir "compiled-page.md.rkt"))
(write-page! page-c "COMPILED ONE")
(managed-compile-zo page-c)
(define page-c-zo (get-compilation-bytecode-file page-c))

(live-cache-root! zo-dir)
(check-regexp-match #rx"COMPILED ONE" (doc-body-string page-c))

(write-page! page-c "COMPILED TWO")
(check-regexp-match #rx"COMPILED TWO" (doc-body-string page-c))
(check-true (file-exists? page-c-zo) "live loads must not delete raco-built bytecode")
(delete-directory/files zo-dir)

;; ---------------------------------------------------------------------------
;; Same constraint, one level down: a raco-compiled DEPENDENCY of a page
;; (e.g. the site's main.rkt) is loaded by rerequire's own handler, which
;; load-doc cannot intercept per-module. The camp-live cache mode keeps
;; that handler blind to raco-built bytecode, so those first declarations
;; also avoid constant enforcement.

(define cdep-dir (make-temporary-directory "load-doc-test-cdep-~a"))
(define chelper (build-path cdep-dir "helper.rkt"))
(define cdep-page (build-path cdep-dir "dep-page.md.rkt"))

(write-helper! chelper "COMPILED DEP V1")
(write-dep-page! cdep-page "•greeting[]")
;; Compile in a subprocess, as raco setup would: compiling in this process
;; would load the helper's declaration here and defeat the point of the test
(check-true (system* (build-path (find-console-bin-dir) "raco")
                     "make" (path->string cdep-page)))

(live-cache-root! cdep-dir)
(check-regexp-match #rx"COMPILED DEP V1" (doc-body-string cdep-page))

(write-helper! chelper "COMPILED DEP V2")
(check-regexp-match #rx"COMPILED DEP V2" (doc-body-string cdep-page))
(delete-directory/files cdep-dir)

;; ---------------------------------------------------------------------------
;; With the cache boundary set (load-site does this), live loads write
;; camp-live bytecode for the page and its in-site dependencies — the warm
;; cache for later sessions — but never for modules outside the boundary.

(define site-dir (make-temporary-directory "load-doc-test-site-~a"))
(define lib-dir (make-temporary-directory "load-doc-test-lib-~a"))
(define site-helper (build-path site-dir "helper.rkt"))
(define outside-lib (build-path lib-dir "outlib.rkt"))
(define site-page (build-path site-dir "cached-page.md.rkt"))

(write-mod! outside-lib
            "#lang racket/base\n(provide twice)\n(define (twice s) (string-append s s))\n")
(write-mod! site-helper
            (format "#lang racket/base\n(require (file ~s))\n(provide greeting)\n(define (greeting) (twice \"IN-SITE \"))\n"
                    (path->string outside-lib)))
(write-dep-page! site-page "•greeting[]")

(live-cache-root! site-dir)
(check-regexp-match #rx"IN-SITE IN-SITE" (doc-body-string site-page))
(check-true (live-zo-exists? site-page) "live loads must cache page bytecode")
(check-true (live-zo-exists? site-helper) "live loads must cache in-site dependencies")
(check-false (live-zo-exists? outside-lib) "live cache must not extend outside the boundary")
(delete-directory/files site-dir)
(delete-directory/files lib-dir)

(live-reload? #f)
