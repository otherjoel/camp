#lang racket/base

;; This module provides bindings for Punct source files in camp-demo.
;; Use: #lang punct camp-demo
;;
;; This makes camp's cross-reference functions and collection API available
;; to all Punct source files in this package.

(require camp/xref
         camp)

(provide (all-from-out camp/xref)
         (all-from-out camp))
