#lang racket/base

(require camp
         camp/page
         punct/doc
         punct/fetch
         racket/list)

(provide render-page
         render-post)

;; Shared page layout
(define (layout title body-content)
  `(html
    (head
     (meta ((charset "utf-8")))
     (meta ((name "viewport")
            (content "width=device-width, initial-scale=1")))
     (title ,title " - The River City Reader")
     (link ((rel "stylesheet") (href "/style.css")))
     (link ((rel "alternate")
            (type "application/atom+xml")
            (href "/feed.atom"))))
    (body
     (header
      (h1 (a ((href "/")) "The River City Reader"))
      (nav
       (a ((href "/blog/")) "Blog")
       " | "
       (a ((href "/tags/")) "Tags")
       " | "
       (a ((href "/about/")) "About")))
     (main ,@body-content)
     (footer
      (p "© 1912 Marian Paroo. River City, Iowa.")))))

;; Render function for static pages
;; Handles both Punct documents and #lang camp/page documents
(define (render-page doc context)
  (define title (or (meta-ref doc 'title) "Untitled"))
  (define body (hash-ref context 'body))
  (define slug (hash-ref context 'slug))

  (cond
    ;; #lang camp/page documents: body is already fully formed
    ;; Just wrap in the site layout
    [(camp-page-doc? doc)
     (layout title `((article ,@body)))]

    ;; Punct documents: add title and any special content
    [else
     ;; Special handling for home page: show recent posts
     (define extra-content
       (if (equal? slug "home")
           (let ([recent-posts (get-collection "blog" #:limit 5)])
             `((section ((class "recent-posts"))
                (h2 "Recent Posts")
                (ul
                 ,@(for/list ([p recent-posts])
                     `(li (a ((href ,(page-link-url p)))
                             ,(page-link-title p))))))))
           '()))

     (layout title
             `((article
                (h1 ,title)
                ,@body
                ,@extra-content)))]))

;; Render function for blog posts
(define (render-post doc context)
  (define title (or (meta-ref doc 'title) "Untitled"))
  (define date (meta-ref doc 'date))
  (define body (hash-ref context 'body))
  (define tags (hash-ref (hash-ref context 'taxonomies) "tags" '()))
  (define series (hash-ref (hash-ref context 'taxonomies) "series" '()))
  (define prev (hash-ref context 'prev))
  (define next (hash-ref context 'next))

  (layout title
          `((article ((class "post"))
             (header
              (h1 ,title)
              ,(if date
                   `(time ((datetime ,(~a date))) ,(format-date date))
                   "")
              ,(if (null? tags)
                   ""
                   `(p ((class "tags"))
                       "Tags: "
                       ,@(add-between
                          (for/list ([tag tags])
                            `(a ((href ,(string-append "/tags/#" tag)))
                                ,tag))
                          ", ")))
              ,(if (null? series)
                   ""
                   `(p ((class "series"))
                       "Part of: " ,(car series))))

             (div ((class "content"))
                  ,@body)

             (nav ((class "pagination"))
                  ,(let ([p (prev)])
                     (if p
                         `(a ((href ,(page-link-url p))
                              (rel "prev"))
                             "← " ,(page-link-title p))
                         ""))
                  ,(let ([n (next)])
                     (if n
                         `(a ((href ,(page-link-url n))
                              (rel "next"))
                             ,(page-link-title n) " →")
                         "")))))))

;; Date formatting helper
(define (format-date d)
  ;; Assuming gregor date object
  (~a d))

(define (~a x)
  (format "~a" x))
