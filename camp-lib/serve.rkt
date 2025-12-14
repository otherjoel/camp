#lang racket/base

(require racket/contract
         "private/serve.rkt")

(provide/contract
 [start-server (->* (path-string?)
                    (#:port exact-nonnegative-integer?
                     #:watch? boolean?)
                    void?)])
