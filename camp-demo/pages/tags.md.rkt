#lang camp/page

#:title "Browse by Tag"
#:slug "tags"

(define tag-pages (get-taxonomy-pages "blog" "tags"))

`((h1 "Browse by Tag")
  (div ((class "content"))
       (p "Browse all posts organized by topic:")
       (div ((class "taxonomy-listing"))
            ,@(for/list ([tag (get-taxonomy-terms "blog" "tags")])
                `(section ((class "tag-section"))
                          (h2 ,tag)
                          (ul
                           ,@(for/list ([pg (hash-ref tag-pages tag)])
                               `(li (a ((href ,(page-link-url pg)))
                                       ,(page-link-title pg))))))))))
