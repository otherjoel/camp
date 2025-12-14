#lang punct camp-demo

---
title: Browse by Tag
slug: tags
---

Browse all posts organized by topic:

•(let ([tag-pages (get-taxonomy-pages "blog" "tags")])
   `(div ((block "root") (class "taxonomy-listing"))
      ,@(for/list ([tag (get-taxonomy-terms "blog" "tags")])
          `(section ()
             (h2 () ,tag)
             (ul ()
               ,@(for/list ([pg (hash-ref tag-pages tag)])
                   `(li () (a ((href ,(page-link-url pg)))
                              ,(page-link-title pg)))))))))
