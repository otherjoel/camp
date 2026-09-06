#lang racket/base

;; NSApplication appearance override (macOS): 'light, 'dark, or #f to follow
;; the system. System-drawn parts — the selection color, scrollbars, window
;; chrome — then match an explicit Light/Dark mode instead of the system.

(require ffi/unsafe/nsstring
         ffi/unsafe/objc)

(provide set-app-appearance!)

(import-class NSApplication NSAppearance)

(define (set-app-appearance! mode)
  (define name
    (case mode
      [(light) "NSAppearanceNameAqua"]
      [(dark) "NSAppearanceNameDarkAqua"]
      [else #f]))
  (tellv (tell NSApplication sharedApplication) setAppearance:
         (and name (tell NSAppearance appearanceNamed: #:type _NSString name))))
