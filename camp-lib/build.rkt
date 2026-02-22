#lang racket/base

(require racket/contract
         "private/structs.rkt"
         "private/build.rkt"
         "private/main.rkt")

(provide/contract
 [collect (-> site? site-info?)]
 [build! (-> site? site-info? void?)])

(provide current-site-info
         (struct-out page)
         (struct-out site-info))