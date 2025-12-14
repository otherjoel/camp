#lang punct camp-demo

---
title: Browse by Series
slug: series
---

Some posts are part of ongoing series:

•(let ([series-pages (get-taxonomy-pages "blog" "series")])
   `(div ((block "root") (class "taxonomy-listing"))
      ,@(for/list ([s (get-taxonomy-terms "blog" "series")])
          `(section ()
             (h2 () ,s)
             (ul ()
               ,@(for/list ([pg (hash-ref series-pages s)])
                   `(li () (a ((href ,(page-link-url pg)))
                              ,(page-link-title pg)))))))))
