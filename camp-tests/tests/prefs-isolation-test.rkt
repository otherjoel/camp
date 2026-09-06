#lang racket/base

;; Framework preference isolation (design §2): requiring
;; camp/app/private/framework-prefs must redirect all framework preference
;; storage to camp-framework-prefs.rktd, leaving DrRacket's shared
;; org.racket-lang.prefs.rktd untouched apart from the three benign keys
;; framework itself may write during unit instantiation.
;;
;; Framework instantiation is process-global, so each check runs in a racket
;; subprocess whose PLTUSERHOME points at a scratch directory — giving it a
;; pristine, throwaway pref-dir while racket-subprocess-env keeps user-scope
;; packages (camp itself) resolvable.

(require racket/file
         racket/port
         racket/system
         rackunit
         rackunit/text-ui
         camp/app/private/subprocess-env)

(define racket-bin (find-system-path 'exec-file))

(define (run-sandboxed home . args)
  (define env (racket-subprocess-env))
  (environment-variables-set! env #"PLTUSERHOME" (path->bytes home))
  (parameterize ([current-environment-variables env])
    (with-output-to-string
      (λ () (unless (apply system* racket-bin args)
              (error 'prefs-isolation-test "subprocess failed: ~a" args))))))

(define (sandboxed-pref-dir home)
  (string->path (run-sandboxed home "-e" "(display (find-system-path 'pref-dir))")))

;; A prefs file is a read-able list of (key value) pairs
(define (read-prefs-file f)
  (if (file-exists? f) (with-input-from-file f read) '()))

(define marker-key 'plt:framework-pref:camp:prefs-isolation-test-marker)

(define instantiation-window-keys
  '(plt:framework-pref:framework:white-on-black?
    plt:framework-pref:framework:exit-when-no-frames
    plt:framework-pref:framework:standard-style-list:font-name))

(define set-marker-program
  '(begin
     (require camp/app/private/framework-prefs framework/preferences)
     (preferences:set-default 'camp:prefs-isolation-test-marker 'unset symbol?)
     (preferences:set 'camp:prefs-isolation-test-marker 'marked)
     (exit 0)))

(define prefs-isolation-tests
  (test-suite
   "framework preference isolation"

   (test-case "framework prefs are redirected to camp-framework-prefs.rktd"
     (define home (make-temporary-directory "camp-prefs-isolation-~a"))
     (dynamic-wind
      void
      (λ ()
        (define pref-dir (sandboxed-pref-dir home))
        (define shared-file (build-path pref-dir "org.racket-lang.prefs.rktd"))
        (define camp-file (build-path pref-dir "camp-framework-prefs.rktd"))
        ;; pre-seed the shared file so preservation is actually exercised
        (make-directory* pref-dir)
        (put-preferences '(camp:prefs-isolation-test-sentinel) '(preexisting)
                         #f shared-file)
        (define before (read-prefs-file shared-file))

        (run-sandboxed home "-e" (format "~s" set-marker-program))

        (check-true (file-exists? camp-file)
                    "camp-framework-prefs.rktd was created")
        (define camp-prefs (read-prefs-file camp-file))
        (check-equal? (assq marker-key camp-prefs)
                      (list marker-key 'marked)
                      "marker landed in camp-framework-prefs.rktd")

        (define after (read-prefs-file shared-file))
        (check-false (assq marker-key after)
                     "marker is absent from the shared prefs file")
        ;; every prior key survives unchanged
        (for ([pair (in-list before)])
          (check-equal? (assq (car pair) after) pair
                        (format "shared pref preserved: ~a" (car pair))))
        ;; anything added or changed is one of the documented
        ;; instantiation-window keys (design §2)
        (for ([pair (in-list after)])
          (unless (member pair before)
            (check-not-false (memq (car pair) instantiation-window-keys)
                             (format "unexpected shared-prefs write: ~a" pair)))))
      (λ () (delete-directory/files home))))))

(module+ main
  (run-tests prefs-isolation-tests))

(module+ test
  (run-tests prefs-isolation-tests))
