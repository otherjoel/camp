#lang racket/base

(require racket/string)

(provide racket-subprocess-env)

;; The app may be launched by Finder with its Racket context (addon dir,
;; compiled-file roots) supplied as launcher flags rather than environment
;; variables (see ../install.rkt). Subprocesses that run racket or raco —
;; the full rebuild, a deploy script — need that context restated in their
;; environment, or raco will miss user-scope packages such as camp itself.
(define (racket-subprocess-env)
  (define env (environment-variables-copy (current-environment-variables)))
  (environment-variables-set! env #"PLTADDONDIR"
                              (path->bytes (find-system-path 'addon-dir)))
  (define roots (current-compiled-file-roots))
  (unless (equal? roots '(same))
    (define sep (if (eq? (system-type 'os) 'windows) ";" ":"))
    (environment-variables-set!
     env #"PLTCOMPILEDROOTS"
     (string->bytes/utf-8
      (string-join (for/list ([r (in-list roots)])
                     (if (eq? r 'same) "." (path->string r)))
                   sep))))
  env)
