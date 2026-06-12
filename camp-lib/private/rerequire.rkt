#lang racket/base

(require compiler/cm
         compiler/compilation-path
         file/sha1
         racket/path
         racket/promise
         racket/rerequire
         syntax/modresolve)

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
;; manager records them as dependencies without compiling them. The stamp
;; reflects the source only: installed bytecode must not mask source edits,
;; since live loading ignores that bytecode. The sha1 makes edits within
;; one mtime second still register.
(define ((skip-outside entry) path)
  (define p (simple-form-path path))
  (and (not (equal? p entry))
       (not (in-cache? p))
       (if (file-exists? p)
           (cons (file-or-directory-modify-seconds p)
                 (delay/sync (call-with-input-file p sha1)))
           (cons -inf.0 ""))))

;; Compilation runs in a private namespace: instantiating a module here (for
;; a dependent's expansion) must not declare it in the live namespace, where
;; it would escape rerequire's dependency tracking and never reload. The
;; namespace persists so the language chain instantiates once — but when a
;; module already declared here is recompiled (a shared helper was edited),
;; dependents must not expand against the stale declaration: start over in a
;; fresh namespace, where every declaration is then current.
(define compile-namespace (box (make-base-namespace)))

(struct exn:stale-namespace ())

(define (compile-live! path)
  (let retry ()
    (define ns (unbox compile-namespace))
    (with-handlers ([exn:stale-namespace?
                     (λ (_e)
                       (set-box! compile-namespace (make-base-namespace))
                       (retry))])
      (parameterize* ([use-compiled-file-paths (cons live-mode (use-compiled-file-paths))]
                      [current-namespace ns]
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
