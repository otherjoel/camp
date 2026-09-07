#lang racket/base

;; Frame placement on screen: a window's frame as (x y w h), putting its
;; top-left at a point, the attached screens' frames, and the primary
;; screen's usable size, all in pixels right and down from the primary
;; screen's top-left corner. macOS goes through Cocoa (see mac-geometry);
;; elsewhere racket/gui's own frame coordinates serve.

(require racket/class
         racket/gui/base)

(provide window-frame
         set-window-top-left!
         screen-frames
         primary-visible-size)

(define-values (window-frame set-window-top-left! screen-frames primary-visible-size)
  (cond
    [(eq? (system-type 'os) 'macosx)
     (apply values
            (for/list ([name '(window-frame set-window-top-left! screen-frames primary-visible-size)])
              (dynamic-require 'camp/app/private/mac-geometry name)))]
    [else
     (values
      (λ (f) (list (send f get-x) (send f get-y) (send f get-width) (send f get-height)))
      (λ (f x y) (send f move x y))
      (λ ()
        (for/list ([m (in-range (get-display-count))])
          (define-values (ix iy) (get-display-left-top-inset #f #:monitor m))
          (define-values (w h) (get-display-size #t #:monitor m))
          (list (- ix) (- iy) w h)))
      (λ () (get-display-size)))]))
