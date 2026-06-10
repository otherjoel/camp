#lang racket/base

(require compiler/compilation-path
         racket/file
         racket/path
         racket/rerequire
         racket/string
         syntax/modresolve)

(provide live-reload?
         rerequire!
         clear-site-bytecode!)

;; Bytecode produced by raco setup/make is compiled with constant enforcement,
;; so a module first declared in a process from its .zo can never be
;; redeclared by dynamic-rerequire ("cannot re-define a constant", after which
;; rerequire silently serves the stale module). In a live-reload process (the
;; GUI app, raco camp serve) site modules must therefore be first-declared
;; from source, which means deleting their bytecode; one-shot builds never
;; redeclare, so they keep the compile cache. All loading of site modules
;; must go through rerequire! (or load-doc/load-site, which use it).

(define live-reload? (make-parameter #f))

(define (rerequire! mod)
  (when (live-reload?)
    (define path
      (with-handlers ([exn:fail? (λ (_) #f)])
        (if (path? mod) mod (resolve-module-path mod #f))))
    (when (path? path)
      (for* ([mode (in-list (use-compiled-file-paths))]
             [root (in-list (current-compiled-file-roots))])
        (define zo (get-compilation-bytecode-file path #:modes (list mode) #:roots (list root)))
        (when (file-exists? zo)
          (with-handlers ([exn:fail:filesystem? void])
            (delete-file zo))))))
  (dynamic-rerequire mod))

;; Removes all compiled bytecode under a site root (except within skip-dirs,
;; e.g. the output and static folders). rerequire! only covers modules it is
;; called on directly; their dependencies (a site's main.rkt, say) are loaded
;; by rerequire's own handler from whatever bytecode exists, so a live-reload
;; session must sweep the whole site tree up front. load-site does this when
;; live-reload? is on.
(define (clear-site-bytecode! root [skip-dirs '()])
  (when (directory-exists? root)
    (define skip (map simplify-path skip-dirs))
    (define (name-of p) (path->string (file-name-from-path p)))
    (define (descend? d)
      (and (not (string-prefix? (name-of d) "."))
           (not (string=? (name-of d) "compiled"))
           (not (member (simplify-path d) skip))))
    (for ([p (in-directory root descend?)]
          #:when (and (directory-exists? p)
                      (string=? (name-of p) "compiled")))
      (with-handlers ([exn:fail:filesystem? void])
        (delete-directory/files p)))))
