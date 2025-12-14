#lang racket/base

(require gregor
         racket/contract
         racket/list
         racket/path
         racket/string)

(provide source-path-pattern?
         output-path-pattern?
         file-extension?
         non-rkt-file-extension?
         (contract-out
          [format-output-path (-> output-path-pattern?
                                  string?
                                  (or/c date-provider? #f)
                                  path?)]))

;; ---------------------------------------------------------------------------
;; Internal helpers

(define *wildcard (string->path "*"))

(define (wildcard? p)
  (equal? *wildcard (if (path? p) p (string->path p))))

(define (extract-bracketed-patterns str) ; "[yyyy]-[MM]" -> '("yyyy" "MM")
  (regexp-match* #rx"\\[([^][]+)\\]" str #:match-select cadr))

(define (contains-brackets? str)
  (regexp-match? #rx"\\[[^][]+\\]" str))

;; Validates CLDR patterns; rejects tz-requiring patterns since we only have dates
(define (all-brackets-valid-cldr? str)
  (define patterns (extract-bracketed-patterns str))
  (for/and ([pattern (in-list patterns)])
    (with-handlers ([exn:gregor:invalid-pattern? (λ (_) #f)]
                    [exn:fail:contract? (λ (_) #f)])
      (~t (date 2025 1 15) pattern)
      #t)))

(define (has-date-patterns? path-str)
  (regexp-match? #rx"\\[[^][]+\\]" path-str))

;; ---------------------------------------------------------------------------
;; Contracts

(define source-path-pattern?
  (flat-contract-with-explanation
   (λ (v)
     (cond
       [(and (relative-path? v)
             (equal? (if (string? v) (string->path v) v) (simplify-path v))
             (wildcard? (last (explode-path v))))]
       [else
        (λ (blame)
          (raise-blame-error
           blame v
           '(expected: "Relative path string ending with * and not containing . or .."
             given: "~e") v))]))
   #:name 'source-path-pattern?))

(define output-path-pattern?
  (flat-contract-with-explanation
   (λ (v)
     (cond
       [(not (relative-path? v))
        (λ (blame)
          (raise-blame-error blame v '(expected: "relative path" given: "~e") v))]
       [(not (equal? (if (string? v) (string->path v) v) (simplify-path v)))
        (λ (blame)
          (raise-blame-error blame v '(expected: "path without . or .." given: "~e") v))]
       [(not (for/or ([part (in-list (explode-path v))])
               (wildcard? part)))
        (λ (blame)
          (raise-blame-error blame v '(expected: "path containing * element" given: "~e") v))]
       [(not (for/and ([part (in-list (map path->string (explode-path v)))])
               (all-brackets-valid-cldr? part)))
        (λ (blame)
          (raise-blame-error
           blame v
           '(expected: "bracketed patterns to be valid CLDR date formats"
             given: "~e") v))]
       [else #t]))
   #:name 'output-path-pattern?))

(define file-extension?
  (flat-contract-with-explanation
   (λ (v)
     (cond
       [(and (or (string? v) (bytes? v))
             (regexp-match? #px"^\\.(?!\\.)(?:[^\\\\\\/\r\n.]|\\.[^\\\\\\/\r\n.])+$" v))]
       [else
        (λ (blame)
          (raise-blame-error
           blame v
           '(expected: "String or bytes starting with '.' and containing no directory separators"
             given: "~e") v))]))
   #:name 'file-extension?))

(define non-rkt-file-extension?
  (flat-contract-with-explanation
   (λ (v)
     (cond
       [(and (file-extension? v)
             (not (equal? v #".rkt"))
             (not (equal? v ".rkt")))]
       [else
        (λ (blame)
          (raise-blame-error
           blame v
           '(expected: "String or bytes starting with '.' (other than '.rkt')"
             given: "~e") v))]))
   #:name 'non-rkt-file-extension?))

;; ---------------------------------------------------------------------------
;; Path formatting

(define (format-brackets str date-val)
  (regexp-replace* #rx"\\[([^][]+)\\]"
                   str
                   (λ (full pattern)
                     (~t date-val pattern))))

(define (format-output-path pattern slug date-val)
  (define p (string->path pattern))
  (define-values (_base _name final-slash?) (split-path p))

  (when (and (not date-val) (has-date-patterns? pattern))
    (error 'format-output-path
           "output pattern ~s contains date codes but no date was provided"
           pattern))

  (define parts (explode-path (simplify-path p #f)))
  (define formatted-parts
    (for/list ([part (in-list parts)])
      (define part-str (path->string part))
      (string->path
       (cond
         [(wildcard? part) slug]
         [(and (contains-brackets? part-str) date-val)
          (format-brackets part-str date-val)]
         [else part-str]))))

  (define new-path (apply build-path formatted-parts))

  (cond
    [final-slash? (build-path new-path "index.html")]
    [else (path-add-extension new-path #".html")]))
