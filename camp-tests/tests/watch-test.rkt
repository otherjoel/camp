#lang racket/base

;; Tests for the file watcher, in particular thunk-valued watch paths,
;; which let a long-running watcher pick up path changes (new files,
;; reconfigured source folders) without being restarted.

(require rackunit
         racket/file
         (only-in camp/private/watch start-watcher!))

;; ---------------------------------------------------------------------------
;; List-valued paths (CLI usage): starts and stops cleanly

(define quiet-dir (make-temporary-directory "watch-test-quiet-~a"))
(define stop-quiet (start-watcher! (list quiet-dir) void))
(check-pred procedure? stop-quiet)
(stop-quiet)
(delete-directory/files quiet-dir)

;; ---------------------------------------------------------------------------
;; Thunk-valued paths: changes under the enumerated paths are reported

(define dir (make-temporary-directory "watch-test-~a"))
(define changes (make-channel))
(define stop
  (start-watcher! (λ () (list dir))
                  (λ (p) (channel-put changes p))
                  #:debounce-ms 0))

;; The watcher needs a beat to register its filesystem events; retry with
;; fresh filenames (entry creation reliably triggers a directory event).
(define reported
  (let loop ([tries 0])
    (display-to-file "x" (build-path dir (format "file-~a.txt" tries)))
    (or (sync/timeout 2 changes)
        (and (< tries 4) (loop (add1 tries))))))

(check-pred path? reported "watcher with thunk-valued paths reports changes")
(stop)
(delete-directory/files dir)
