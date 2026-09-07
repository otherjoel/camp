#lang racket/base

;; NSWindow placement (macOS) in one coordinate system: pixels right and down
;; from the primary screen's top-left corner. racket/gui flips a frame's y
;; against Cocoa's *main* screen, which follows the key window, so on a
;; multi-monitor Mac its move and get-y disagree with each other from one
;; call to the next.

(require (only-in ffi/unsafe define-cstruct _double _ulong)
         ffi/unsafe/objc
         racket/class)

(provide window-frame
         set-window-top-left!
         screen-frames
         primary-visible-size)

(define-cstruct _NSPoint ([x _double] [y _double]))
(define-cstruct _NSSize ([w _double] [h _double]))
(define-cstruct _NSRect ([origin _NSPoint] [size _NSSize]))

(import-class NSScreen)

(define (screens)
  (define arr (tell NSScreen screens))
  (for/list ([i (in-range (tell #:type _ulong arr count))])
    (tell arr objectAtIndex: #:type _ulong i)))

(define (primary-height)
  (NSSize-h (NSRect-size (tell #:type _NSRect (car (screens)) frame))))

;; An NSRect (origin at the bottom-left, y up) as (x y w h)
(define (rect->list r)
  (define o (NSRect-origin r))
  (define s (NSRect-size r))
  (for/list ([v (list (NSPoint-x o)
                      (- (primary-height) (NSPoint-y o) (NSSize-h s))
                      (NSSize-w s)
                      (NSSize-h s))])
    (inexact->exact (round v))))

(define (window-frame f)
  (rect->list (tell #:type _NSRect (send f get-handle) frame)))

(define (set-window-top-left! f x y)
  (tellv (send f get-handle) setFrameTopLeftPoint:
         #:type _NSPoint (make-NSPoint (exact->inexact x)
                                       (exact->inexact (- (primary-height) y)))))

(define (screen-frames)
  (for/list ([s (in-list (screens))])
    (rect->list (tell #:type _NSRect s frame))))

(define (primary-visible-size)
  (apply values (cddr (rect->list (tell #:type _NSRect (car (screens)) visibleFrame)))))
