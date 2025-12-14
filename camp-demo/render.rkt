#lang racket/base

(require camp
         punct/doc)

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
(define (render-page doc context)
  (define title (meta-ref doc 'title "Untitled"))
  (define body (hash-ref context 'body))
  (layout title
          `((article
             (h1 ,title)
             ,@body))))

;; Render function for blog posts
(define (render-post doc context)
  (define title (meta-ref doc 'title "Untitled"))
  (define date (meta-ref doc 'date #f))
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
