#lang racket/base

;; Tests for the environment the GUI app gives subprocesses that run racket

(require rackunit
         rackunit/text-ui
         camp/app/private/subprocess-env)

(define subprocess-env-tests
  (test-suite
   "racket-subprocess-env"

   (test-case "sets PLTADDONDIR to this process's addon dir"
     (define env (racket-subprocess-env))
     (check-equal? (environment-variables-ref env #"PLTADDONDIR")
                   (path->bytes (find-system-path 'addon-dir))))

   (test-case "serializes non-default compiled-file roots"
     (parameterize ([current-compiled-file-roots
                     (list (build-path "compiled" "9.2-cs") 'same)])
       (define env (racket-subprocess-env))
       (check-equal? (environment-variables-ref env #"PLTCOMPILEDROOTS")
                     #"compiled/9.2-cs:.")))

   (test-case "leaves PLTCOMPILEDROOTS inherited for default roots"
     (parameterize ([current-compiled-file-roots '(same)])
       (define env (racket-subprocess-env))
       (define inherited (getenv "PLTCOMPILEDROOTS"))
       (check-equal? (environment-variables-ref env #"PLTCOMPILEDROOTS")
                     (and inherited (string->bytes/utf-8 inherited)))))))

(module+ main
  (run-tests subprocess-env-tests))

(module+ test
  (run-tests subprocess-env-tests))
