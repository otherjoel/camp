#lang racket/base

;; Font slots for the built-in editor (design §5): three global (face size)
;; slots cycled with ⌘⌥F. Applying the active slot rewrites the "Standard"
;; style in the shared style list — framework's canonical restyle, so every
;; open editor follows at once — and never touches the framework font
;; preferences, so a #f face or size recovers the stock default forever.

(require camp/app/private/framework-prefs
         framework
         racket/class
         racket/gui
         racket/gui/easy
         racket/gui/easy/operator
         racket/list
         racket/match
         camp/app/private/editor-utils
         camp/app/private/gui
         camp/app/private/settings)

(provide apply-active-font-slot!
         cycle-font-slot!
         slot-font
         @gutter-font)

;; ============================================================================
;; Slot resolution

(define (resolve-face face #:warn? [warn? #f])
  (cond
    [(and face (member face (get-face-list))) face]
    [else
     (when (and face warn?)
       (log-msg "Font ~a is not installed; using the default face" face))
     (preferences:get 'framework:standard-style-list:font-name)]))

(define (resolve-size size)
  (or size (editor:get-current-preferred-font-size)))

(define (slot-font idx)
  (match-define (list face size) (list-ref (obs-peek @font-slots) idx))
  (send the-font-list find-or-create-font
        (resolve-size size) (resolve-face face) 'modern 'normal 'normal))

;; ============================================================================
;; Application

;; The delta dance from framework's update-standard-style
;; (gui-lib/framework/private/editor-misc.rkt): copy the style's delta,
;; adjust face and size, write it back
(define (apply-active-font-slot!)
  (match-define (list face size)
    (list-ref (obs-peek @font-slots) (obs-peek @font-slot)))
  (define standard
    (send (editor:get-standard-style-list) find-named-style "Standard"))
  (when standard
    (define delta (make-object style-delta%))
    (send standard get-delta delta)
    (send delta set-delta-face (resolve-face face #:warn? #t))
    (send delta set-family 'modern)
    (send delta set-size-mult 0)
    (send delta set-size-add (resolve-size size))
    (send standard set-delta delta)))

(define (cycle-font-slot!)
  (@font-slot . <~ . next-font-slot))

(obs-observe! @font-slot (λ (_) (apply-active-font-slot!)))
(obs-observe! @font-slots (λ (_) (apply-active-font-slot!)))

;; The line-number gutter follows the active slot at 70% size (min 7)
(define @gutter-font
  (obs-combine
   (λ (slots idx)
     (match-define (list face size) (list-ref slots idx))
     (send the-font-list find-or-create-font
           (gutter-font-size (resolve-size size))
           (resolve-face face)
           'modern 'normal 'normal))
   @font-slots @font-slot))
