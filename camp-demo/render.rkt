#lang racket/base

(require camp
         camp/page
         punct/doc
         punct/fetch
         racket/list
         racket/string)

(provide render-page
         render-post
         render-book)

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
(define (render-page doc ctxt)
  (define title (or (meta-ref doc 'title) "Untitled"))
  (define body (camp-doc->html-xexpr doc))
  (define slug (hash-ref ctxt 'slug))

  (cond
    ;; #lang camp/page documents: body is already fully formed
    ;; Just wrap in the site layout
    [(camp-page-doc? doc)
     (layout title `((article ,@body)))]

    ;; Punct documents: add title and wrap body in .content
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
                (div ((class "content"))
                     ,@body)
                ,@extra-content)))]))

;; Render function for blog posts
(define (render-post doc ctxt)
  (define title (or (meta-ref doc 'title) "Untitled"))
  (define date (meta-ref doc 'date))
  (define body (camp-doc->html-xexpr doc))
  (define tags (hash-ref (hash-ref ctxt 'taxonomies) "tags" '()))
  (define series (hash-ref (hash-ref ctxt 'taxonomies) "series" '()))

  (layout title
          `((article ((class "post"))
             (header
              (h1 ,title)
              ,(if date
                   `(time ((datetime ,(~d "yyyy-MM-dd" date))) ,(~d "d MMM yyyy" date))
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
                  ,(let ([p (prev ctxt)])
                     (if p
                         `(a ((href ,(page-link-url p))
                              (rel "prev"))
                             ,(page-link-title p))
                         ""))
                  ,(let ([n (next ctxt)])
                     (if n
                         `(a ((href ,(page-link-url n))
                              (rel "next"))
                             ,(page-link-title n))
                         "")))))))

;; ---------------------------------------------------------------------------
;; Book Rendering

(define (render-book parts)
  (string-append
   ;; Preamble: import template and apply with metadata
   #<<TYPST
#import "template.typ": book, term, term_definition

#show: book.with(
  title: "The River City Reader",
  subtitle: "Collected Writings, Summer 1912",
  authors: ("Marian Paroo",),
  publisher: "River City Library Press",
  year: 1912,
  dedication: "For the children of River City.",
  copyright-notice: "Public Domain",
  copyright-legal: "These writings may be freely reproduced.",
)

TYPST
   ;; Render each part
   (string-append*
    (for/list ([part (in-list parts)])
      (render-book-part part)))))

(define (render-book-part part)
  (define name (hash-ref part 'name))
  (define chapters (hash-ref part 'chapters))
  (string-append
   ;(format "= ~a\n\n" (escape-typst name))
   (string-append*
    (for/list ([ch (in-list chapters)])
      (render-book-chapter ch)))))

(define (render-book-chapter ch)
  (define slug (hash-ref ch 'slug))
  (define doc (hash-ref ch 'doc))
  (define title (or (meta-ref doc 'title) slug))
  (string-append
   (format "= ~a <~a>\n\n" (escape-typst title) slug)
   (camp-doc->typst doc)
   "\n\n"))

(define (escape-typst s)
  (regexp-replace* #rx"[#*_@$\\\\]" s "\\\\&"))

