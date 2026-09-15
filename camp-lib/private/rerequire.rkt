#lang racket/base

(require compiler/cm
         compiler/compilation-path
         racket/path
         racket/rerequire
         syntax/modresolve
         "log.rkt")

(provide live-reload?
         live-cache-root!
         rerequire!)

;; Bytecode produced by raco setup/make is compiled with constant enforcement,
;; so a module first declared in a process from its .zo can never be
;; redeclared by dynamic-rerequire ("cannot re-define a constant", after which
;; rerequire silently serves the stale module). In a live-reload process (the
;; GUI app, raco camp serve) site modules therefore live in their own bytecode
;; world: a "camp-live" mode dir, compiled here without constant enforcement.
;; Prepending it to use-compiled-file-paths makes rerequire's loader (which
;; consults only the first mode) blind to raco-built bytecode, while ordinary
;; library loads fall through to the remaining modes. The cache persists
;; across sessions, so only changed sources ever recompile. All loading of
;; site modules must go through rerequire! (or load-doc/load-site, which use
;; it).

(define live-reload? (make-parameter #f))

(define live-mode (build-path "compiled" "camp-live"))

;; The cache boundary, normally the site root (set by load-site). Modules
;; outside it are treated as stable libraries; they are never compiled into
;; the cache, never tracked for reload, and must always enter the live
;; namespace from their installed bytecode.
(define cache-root (box #f))
(define (live-cache-root! root)
  (set-box! cache-root (path->directory-path (simple-form-path root))))

;; split-path returns parent dirs in trailing-slash form, matching cache-root
(define (in-cache? p)
  (define root (unbox cache-root))
  (and root
       (let loop ([p p])
         (or (equal? root p)
             (let-values ([(base _name _dir?) (split-path p)])
               (and (path? base) (loop base)))))))

;; Report a freshness stamp for out-of-boundary modules so the compilation
;; manager records them as dependencies without compiling them. Cached site
;; bytecode links against the library instance the regular loader declares,
;; so the stamp follows that form: the installed bytecode, or the source
;; when it is newer. A library recompiled from unchanged source (a Racket
;; upgrade; raco setup after one of its own dependencies was edited) thereby
;; still invalidates its dependents in the cache.
;;
;; A library recompiled during a session cannot be reloaded (the live
;; namespace keeps its first instance), but cache written from then on must
;; still link against the new bytecode its stamp records, so the next
;; session finds it consistent: dependents are compiled in a fresh
;; compilation namespace (see below) rather than against the stale
;; declaration.
(define stamps (make-hash))

(define ((skip-outside entry) path)
  (define p (simple-form-path path))
  (and (not (equal? p entry))
       (not (in-cache? p))
       (let ([stamp (or (file-stamp-in-paths p (list (car (explode-path p)))) ; any path is under its own root
                        (cons -inf.0 ""))]
             [seen (hash-ref stamps p #f)])
         (hash-set! stamps p stamp)
         (when (and seen (not (equal? (car seen) (car stamp))))
           (log-camp-warning "Recompiled since this session started; restart to use it: ~a" p)
           (raise (exn:stale-namespace)))
         stamp)))

;; Compilation runs in a private namespace: instantiating a module here (for
;; a dependent's expansion) must not declare it in the live namespace, where
;; it would escape rerequire's dependency tracking and never reload. The
;; namespace persists so the language chain instantiates once — but when a
;; module already declared here is recompiled (a shared helper was edited),
;; dependents must not expand against the stale declaration: start over in a
;; fresh namespace, where every declaration is then current.
(define compile-namespace (box (make-base-namespace)))

(struct exn:stale-namespace ())

;; A module declared here is not loaded again when a dependent requires it,
;; so its staleness would pass unnoticed whenever the dependent is compiled
;; outright (its own source changed) rather than after its dependencies:
;; have the compilation manager check such declarations on resolution.
(define (checking-resolver orig)
  (case-lambda
    [(name ns) (orig name ns)]
    [(mod rel stx load?)
     (when load?
       (define name (resolved-module-path-name (orig mod rel stx #f)))
       (define file (if (pair? name) (car name) name))
       (when (path? file)
         (define p (simple-form-path file))
         (when (and (in-cache? p) (module-declared? (make-resolved-module-path p)))
           (managed-compile-zo p))))
     (orig mod rel stx load?)]))

(define (compile-live! path)
  (let retry ()
    (define ns (unbox compile-namespace))
    (with-handlers ([exn:stale-namespace?
                     (λ (_e)
                       (set-box! compile-namespace (make-base-namespace))
                       (retry))])
      (parameterize* ([use-compiled-file-paths (cons live-mode (use-compiled-file-paths))]
                      [current-namespace ns]
                      [current-module-name-resolver (checking-resolver (current-module-name-resolver))]
                      [current-load/use-compiled (make-compilation-manager-load/use-compiled-handler)]
                      [compile-enforce-module-constants #f]
                      [manager-skip-file-handler (skip-outside path)]
                      [manager-compile-notify-handler
                       (λ (p)
                         (define sp (simple-form-path p))
                         (when (and (not (equal? sp path))
                                    (module-declared? (make-resolved-module-path sp)))
                           (raise (exn:stale-namespace))))])
        (managed-compile-zo path)))))

;; ---------------------------------------------------------------------------
;; Library predeclaration
;;
;; Cached bytecode carries cross-module references into the installed
;; bytecode of the libraries it was compiled against. Were rerequire's loader
;; to load such a library, it would compile it from source (the camp-live
;; mode has no bytecode for it) and the instances would not match. Declaring
;; every out-of-boundary import through the regular loader first keeps
;; library loading on installed bytecode; in-site imports are left for
;; rerequire to load and track.

(define (live-zo-path path)
  (for/or ([root (in-list (current-compiled-file-roots))])
    (define zo (get-compilation-bytecode-file path #:modes (list live-mode) #:roots (list root)))
    (and (file-exists? zo) zo)))

(define (compiled-imports code)
  (let loop ([c code] [acc '()])
    (define imports (apply append (map cdr (module-compiled-imports c))))
    (for*/fold ([acc (append imports acc)])
               ([non-star? (in-list '(#t #f))]
                [sub (in-list (module-compiled-submodules c non-star?))])
      (loop sub acc))))

(define (import-base-path mpi wrt)
  (define r (with-handlers ([exn:fail? (λ (_) #f)])
              (resolve-module-path-index mpi wrt)))
  (let base ([r r])
    (cond
      [(path? r) (simple-form-path r)]
      [(and (pair? r) (eq? (car r) 'submod))
       (base (if (path? (cadr r)) (cadr r) wrt))]
      [else #f])))

(define (predeclare-libraries! path)
  (define seen (make-hash))
  (let loop ([p path])
    (unless (hash-ref seen p #f)
      (hash-set! seen p #t)
      (define zo (live-zo-path p))
      (when zo
        (define code
          (parameterize ([read-accept-compiled #t])
            (call-with-input-file zo read)))
        (for ([mpi (in-list (compiled-imports code))])
          (define dep (import-base-path mpi p))
          (when dep
            (cond
              [(in-cache? dep) (loop dep)]
              [(hash-ref seen dep #f) (void)]
              [else
               (hash-set! seen dep #t)
               (with-handlers ([exn:fail? void])
                 (dynamic-require dep (void)))])))))))

;; ---------------------------------------------------------------------------

(define (rerequire! mod)
  (cond
    [(live-reload?)
     (define path
       (with-handlers ([exn:fail? (λ (_) #f)])
         (define p (if (path? mod) mod (resolve-module-path mod #f)))
         (and (path? p) (simple-form-path p))))
     (when path
       (compile-live! path)
       (predeclare-libraries! path))
     (parameterize ([use-compiled-file-paths (cons live-mode (use-compiled-file-paths))])
       (dynamic-rerequire mod))]
    [else (dynamic-rerequire mod)]))
