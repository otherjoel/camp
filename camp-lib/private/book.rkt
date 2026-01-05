#lang racket/base

(require racket/path
         racket/rerequire)

(provide load-book)

(define (load-book file-path)
  (define abs-path
    (simplify-path
     (path->complete-path
      (if (path? file-path) file-path (string->path file-path)))))
  (dynamic-rerequire abs-path)
  (define book-config (dynamic-require abs-path 'toml))
  (hash-set book-config 'path abs-path))
