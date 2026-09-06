#lang racket/base

;; Shape-pinning for the color-scheme-entry: adjustment tables (design §10.1):
;; camp's direct read-modify-write of the pref hash must produce exactly what
;; framework's public color-prefs:set-in-color-scheme writes for the same
;; value. The comparison is on the raw marshalled datum in the prefs file —
;; the format camp depends on — so upstream drift fails loudly here.
;;
;; Framework state is process-global, so the writes run in a racket
;; subprocess with a sandboxed PLTUSERHOME (as in prefs-isolation-test).

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
              (error 'appearance-test "subprocess failed: ~a" args))))))

(define pinning-program
  '(begin
     (require camp/app/private/appearance
              framework
              racket/class
              racket/draw)
     (define camp-file
       (build-path (find-system-path 'pref-dir) "camp-framework-prefs.rktd"))
     (define (raw entry)
       (get-preference
        (string->symbol (format "plt:framework-pref:color-scheme-entry:~a" entry))
        (λ () #f) 'timestamp camp-file #:use-lock? #f))
     ;; set-in-color-scheme writes the current polarity's slot — force classic
     (preferences:set 'framework:white-on-black-mode? #f)
     (define (pin entry value)
       (color-prefs:set-in-color-scheme entry value)
       (define via-public (raw entry))
       (preferences:set (string->symbol (format "color-scheme-entry:~a" entry))
                        (hash))
       (set-color-scheme-slot! entry 'classic value)
       (list via-public (raw entry)))
     (write
      (list (pin 'camp:vim-selection-color (make-color 12 34 56))
            (pin 'framework:syntax-color:scheme:keyword
                 (value-spec->object (list "#ff79c6" #t #f #t)))))
     (exit 0)))

(define markup-program
  '(begin
     (require camp/app/private/appearance
              framework
              racket/class
              racket/draw)
     (preferences:set 'framework:white-on-black-mode? #f)
     (color-prefs:set-current-color-scheme 'classic)
     (apply-scheme-tables!)
     (define (style name)
       (send (editor:get-standard-style-list) find-named-style name))
     (define (delta entry) (color-prefs:lookup-in-color-scheme entry))
     (define (text-color)
       (define c (send (delta 'framework:syntax-color:scheme:text) get-foreground-add))
       (list (send c get-r) (send c get-g) (send c get-b)))
     (define classic-text (text-color))
     (preferences:set 'framework:white-on-black-mode? #t)
     (color-prefs:set-current-color-scheme 'white-on-black)
     (write
      (list (and (style "framework:syntax-color:scheme:markup-heading") #t)
            (send (delta 'framework:syntax-color:scheme:markup-heading) get-weight-on)
            (send (delta 'framework:syntax-color:scheme:markup-emphasis) get-style-on)
            classic-text
            (text-color)))
     (exit 0)))

(define appearance-tests
  (test-suite
   "color-scheme-entry shape pinning"

   (test-case "markup entries are registered; built-in schemes paint text in the foreground"
     (define home (make-temporary-directory "camp-appearance-~a"))
     (dynamic-wind
      void
      (λ ()
        (define results
          (read (open-input-string
                 (run-sandboxed home "-e" (format "~s" markup-program)))))
        (check-equal? results '(#t bold italic (0 0 0) (255 255 255))))
      (λ () (delete-directory/files home))))

   (test-case "direct slot writes match color-prefs:set-in-color-scheme"
     (define home (make-temporary-directory "camp-appearance-~a"))
     (dynamic-wind
      void
      (λ ()
        (define results
          (read (open-input-string
                 (run-sandboxed home "-e" (format "~s" pinning-program)))))
        (for ([pair (in-list results)]
              [label (in-list '("color entry" "style-delta entry"))])
          (define via-public (car pair))
          (define via-camp (cadr pair))
          (check-pred hash? via-public
                      (format "~a: public write landed in the camp prefs file" label))
          (check-true (hash-has-key? via-public 'classic)
                      (format "~a: classic slot present" label))
          (check-equal? via-camp via-public
                        (format "~a: camp's write matches the public API" label))))
      (λ () (delete-directory/files home))))))

(module+ main
  (run-tests appearance-tests))

(module+ test
  (run-tests appearance-tests))
