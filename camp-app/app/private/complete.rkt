#lang racket/base

;; Completion engine for the in-app editor: a background worker expands
;; buffer text and harvests module-level definitions plus required exports,
;; using walkers vendored from drcomplete (via racket-langserver) in complete/.
;;
;; Expansion runs arbitrary compile-time site code, so each request gets a
;; fresh namespace inside a memory-capped custodian with a watchdog timeout.
;; A request that fails to expand keeps the path's previous completions.

(require racket/async-channel
         racket/list
         racket/path
         racket/set
         syntax/modread
         (prefix-in required: camp/app/private/complete/required)
         (prefix-in user-defined: camp/app/private/complete/user-defined))

(provide request-completions!
         completions-for
         buffer-words)

;; ============================================================================
;; Cache

(define cache (make-hash))
(define cache-sema (make-semaphore 1))

(define (completions-for path)
  (call-with-semaphore cache-sema (λ () (hash-ref cache path '()))))

;; ============================================================================
;; Expansion

(define memory-limit (* 256 1024 1024))
(define time-limit 30)

(define (expand-buffer path text)
  (define in (open-input-string text))
  (port-count-lines! in)
  (parameterize ([current-namespace (make-base-namespace)]
                 [current-load-relative-directory (path-only path)]
                 [current-directory (path-only path)])
    (expand (with-module-reading-parameterization (λ () (read-syntax path in))))))

(define (harvest path text)
  (define cust (make-custodian))
  (custodian-limit-memory cust memory-limit cust)
  (define result #f)
  (define expander
    (parameterize ([current-custodian cust])
      (thread
       (λ ()
         (with-handlers ([(λ (_) #t) void])
           (define stx (expand-buffer path text))
           (set! result
                 (sort (remove-duplicates
                        (map symbol->string
                             (append (set->list (user-defined:walk stx))
                                     (with-handlers ([exn:fail? (λ (_) '())])
                                       (set->list (required:walk-module stx))))))
                       string<?)))))))
  (sync/timeout time-limit expander)
  (custodian-shutdown-all cust)
  result)

;; ============================================================================
;; Worker

(define requests (make-async-channel))

(define (request-completions! path text)
  (async-channel-put requests (cons path text)))

(void
 (thread
  (λ ()
    (let loop ()
      ;; Coalesce the backlog so only the newest request per path expands
      (define backlog
        (let drain ([acc (list (async-channel-get requests))])
          (define r (async-channel-try-get requests))
          (if r (drain (cons r acc)) acc)))
      (define newest
        (for/fold ([h (hash)]) ([req (in-list (reverse backlog))])
          (hash-set h (car req) (cdr req))))
      (for ([(path text) (in-hash newest)])
        (define words (harvest path text))
        (when words
          (call-with-semaphore cache-sema
                               (λ () (hash-set! cache path words)))))
      (loop)))))

;; ============================================================================
;; Lexical fallback

(define (buffer-words text)
  (remove-duplicates
   (regexp-match* #px"[A-Za-z][A-Za-z0-9_!?*<>=/+-]{2,}" text)))
