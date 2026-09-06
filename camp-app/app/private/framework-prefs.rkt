#lang racket/base

;; Instantiates framework, then permanently redirects its preference storage
;; so camp never reads or writes DrRacket's org.racket-lang.prefs.rktd.
;; Must be the first require of any camp module that requires framework.

(require racket/file framework/preferences framework)

(define camp-framework-prefs-file
  (build-path (find-system-path 'pref-dir) "camp-framework-prefs.rktd"))

(preferences:low-level-get-preference
 (λ (sym [fail (λ () #f)])
   (get-preference sym fail 'timestamp camp-framework-prefs-file #:use-lock? #f)))
(preferences:low-level-put-preferences
 (λ (syms vals) (put-preferences syms vals #f camp-framework-prefs-file)))
