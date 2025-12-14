#lang racket/base

(require racket/contract
         "private/structs.rkt"
         "private/build.rkt")

(provide/contract
 [collect (-> site? site-info?)]
 [build! (-> site? site-info? void?)])
