#lang racket/base

(require racket/contract
         punct/doc
         "private/structs.rkt"
         "private/build.rkt"
         "private/main.rkt")

(provide/contract
 [collect (-> site? site-info?)]
 [collect/call-with-page (-> site? string? (-> document? context? any) any)]
 [build! (-> site? site-info? void?)])

(provide current-site-info
         (struct-out page)
         (struct-out site-info))