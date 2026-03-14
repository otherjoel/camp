#lang racket/base

;; ANSI escape sequence parser — decoding counterpart to the color helpers in log.rkt

(require racket/list
         racket/string)

(provide parse-ansi)

(define ansi-re #rx"\033\\[[0-9;]*m")

;; Reverse mapping: ANSI numeric code → style symbol
(define code->style
  (hasheqv 0  #f
           1  'bold
           2  'dim
           31 'red
           32 'green
           33 'yellow
           34 'blue
           35 'magenta
           36 'cyan
           37 'white
           92 'bright-green
           96 'bright-cyan))

;; Extract the numeric code from an escape sequence like "\033[32m"
;; For compound codes like "\033[1;32m", uses the last code.
(define (escape->style esc)
  (define inner (substring esc 2 (sub1 (string-length esc))))
  (define parts (string-split inner ";"))
  (define num (string->number (last parts)))
  (hash-ref code->style (or num -1) #f))

;; Parse a string containing ANSI escape sequences into styled spans.
;; Returns (listof (cons/c string? (or/c symbol? #f)))
(define (parse-ansi str)
  (define parts (regexp-match-positions* ansi-re str))
  (when (null? parts)
    (if (string=? str "")
        (set! parts '())
        (void)))
  (let loop ([pos 0] [matches parts] [style #f] [acc '()])
    (cond
      [(null? matches)
       (define tail (substring str pos))
       (reverse (if (string=? tail "") acc (cons (cons tail style) acc)))]
      [else
       (define m (car matches))
       (define text (substring str pos (car m)))
       (define esc (substring str (car m) (cdr m)))
       (define new-style (escape->style esc))
       (define next-acc (if (string=? text "") acc (cons (cons text style) acc)))
       (loop (cdr m) (cdr matches) new-style next-acc)])))
