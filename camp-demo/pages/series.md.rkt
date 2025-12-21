#lang camp/page

#:title "Browse by Series"
#:slug "series"

(define series-pages (get-taxonomy-pages "blog" "series"))

`((h1 "Browse by Series")
  (div ((class "content"))
       (p "Some posts are part of ongoing series:")
       (div ((class "taxonomy-listing"))
            ,@(for/list ([s (get-taxonomy-terms "blog" "series")])
                `(section ((class "series-section"))
                          (h2 ,s)
                          (ul
                           ,@(for/list ([pg (hash-ref series-pages s)])
                               `(li (a ((href ,(page-link-url pg)))
                                       ,(page-link-title pg))))))))))
