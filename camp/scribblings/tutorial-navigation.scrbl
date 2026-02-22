#lang scribble/manual

@(require (for-label camp
                     camp/xref
                     punct/doc
                     racket/base))

@title[#:tag "tutorial-navigation"]{Tutorial: Navigation and Cross-References}

As your site grows, readers need ways to move between related content. Camp provides several
mechanisms for this: automatic prev/next navigation within collections, a cross-reference system
for linking to term definitions and other pages, and taxonomies for grouping content by tags or
categories. This tutorial explores each of these, building on the blog we created in the previous
chapter.

@section[#:tag "nav-prev-next"]{Previous and Next Links}

Every page in a collection has a position in that collection's sort order. For a blog sorted by
date descending, the first post is the newest, and moving ``next'' means moving to older posts.
Camp makes this navigation available through the @racket[prev] and @racket[next] functions, which
take a render context and return the adjacent @racket[page-link] (or @racket[#f] at collection
boundaries):

@codeblock{
(define (render-post doc ctxt)
  (define prev-page (prev ctxt))
  (define next-page (next ctxt))
  ;; prev-page and next-page are either page-link structs or #f
  ...)
}

When a page link exists, you can extract its URL and title using @racket[page-link-url] and
@racket[page-link-title]. Here's a navigation component you might add to your post template:

@codeblock{
(define (post-navigation ctxt)
  (define p (prev ctxt))
  (define n (next ctxt))
  `(nav ((class "post-nav"))
     ,(if p
          `(a ((href ,(page-link-url p)) (class "prev"))
              "← " ,(page-link-title p))
          `(span))
     ,(if n
          `(a ((href ,(page-link-url n)) (class "next"))
              ,(page-link-title n) " →")
          `(span))))
}

The conditional checks prevent broken links at collection boundaries. You'd call this function from
your render function and insert the result into your template.

@section[#:tag "nav-taxonomies"]{Taxonomies}

Taxonomies group pages by metadata values. A blog might have tags (each post can have multiple) and
a series field (for multi-part articles). Camp discovers these relationships during the collect
phase and makes them available for navigation.

To use taxonomies, declare them in your collection configuration:

@verbatim|{
[[collections]]
name = "posts"
source = "posts/*"
output-paths = "posts/[yyyy]/[MM]/*/"
render-with = 'myblog/render render-post
order = "descending"
sort-key = "date"
taxonomies = ["tags", "series"]
}|

Then add the corresponding metadata to your posts:

@filebox["posts/emacs-intro.md.rkt"]{@codeblock|{
#lang punct

---
title: Getting Started with Emacs
date: 2025-01-15
tags: emacs, editors, productivity
series: Emacs from Scratch
---

This is the first post in a series about learning Emacs...
}|}

Tags are specified as comma-separated values. Camp normalizes these into a list, so
@racket["emacs, editors, productivity"] becomes @racket['("emacs" "editors" "productivity")]. You
can also use Racket's datum syntax if you prefer: @tt{tags: '(emacs editors productivity)}.

The context passed to your render function includes a @racket[taxonomies] field containing the
current page's taxonomy values, already normalized:

@codeblock{
(define (render-post doc ctxt)
  (define tags (hash-ref (context-taxonomies ctxt) "tags" '()))
  ;; tags is now '("emacs" "editors" "productivity")
  ...)
}

@subsection[#:tag "nav-within-taxonomies"]{Navigating Within Taxonomies}

The @racket[prev] and @racket[next] functions accept optional arguments for taxonomy navigation.
With a taxonomy key, they navigate within the first value of that taxonomy for the current page.
With a taxonomy key and a specific term, they navigate within that term's pages.

@codeblock{
;; Navigate within the collection
(prev ctxt)                    ; previous post overall
(next ctxt)                    ; next post overall

;; Navigate within taxonomy
(prev ctxt "tags")             ; previous post with same first tag
(next ctxt "series")           ; next post in same series

;; Navigate within specific term
(prev ctxt "tags" "emacs")     ; previous post tagged "emacs"
(next ctxt "tags" "emacs")     ; next post tagged "emacs"
}

This enables features like ``next post in this series'' links or ``more posts about Emacs''
suggestions. The pages returned are ordered according to the collection's sort settings, so in a
date-descending blog, ``next'' in a tag means the next-oldest post with that tag.

@section[#:tag "nav-term-definitions"]{Term Definitions and References}

Camp includes a cross-reference system inspired by Scribble, Racket's documentation tool. You can
define terms in one document and link to them from anywhere on your site. This is particularly
useful for glossaries, technical writing, or any content where concepts recur across pages.

Term functions live in the @racketmodname[camp/xref] module. Import it in your source documents:

@filebox["posts/api-design.md.rkt"]{@codeblock|{
#lang punct

(require camp/xref)

---
title: REST API Design Principles
date: 2025-02-10
tags: apis, web development
---

A •defterm{REST} (Representational State Transfer) API uses HTTP methods
to perform operations on resources. When designing a REST API, the most
important principle is...
}|}

The @racket[defterm] function marks a term definition. The text
you provide appears in the rendered output and also creates an anchor that other pages can link to.
During rendering, Camp transforms this into a @tt{<dfn>} element with an @tt{id} attribute based on
the normalized term name.

To reference a defined term from another document:

@filebox["posts/microservices.md.rkt"]{@codeblock|{
#lang punct

(require camp/xref)

---
title: Microservices Communication
date: 2025-02-15
tags: apis, architecture
---

Microservices typically communicate via •term{REST} APIs or message queues.
The choice depends on...
}|}

The @racket[term] function creates a link to wherever that term is defined. Camp looks up the
definition during the render phase and generates an appropriate hyperlink. If the term hasn't been
defined anywhere, Camp logs a warning and renders an error marker so you can find and fix the
reference.

@subsection[#:tag "nav-term-normalization"]{Term Normalization}

Camp normalizes term names so that natural variations in prose still match. The normalization
rules handle common cases:

Capitalization doesn't matter: a definition of ``REST'' matches references to ``rest'' or ``Rest.''
Basic pluralization is handled: defining ``API'' allows references to ``APIs,'' and defining
``library'' matches ``libraries.'' Whitespace is normalized to hyphens, so ``REST API'' and
``rest-api'' are equivalent.

This means you can write naturally. If you define ``microservice'' in one post and later write
about ``microservices'' in another, the link will resolve correctly. The system isn't
exhaustive---it won't handle irregular plurals or major spelling variations---but it covers the
common cases that would otherwise require tedious manual normalization.

@section[#:tag "nav-page-refs"]{Page References}

Sometimes you want to link to another page by its identity rather than by URL. The
@racket[page-ref] function creates a link that Camp resolves during rendering:

@verbatim|{
For more details, see •page-ref{api-design}.
}|

The argument is the target page's slug, which defaults to the filename without extension. Spaces
are normalized to hyphens, so @racket[page-ref]{api design} works too.

By default, the link text is the target page's title. To use custom text, provide it as content:

@codeblock{
See •page-ref["api-design"]{my earlier post about REST} for background.
}

The first argument (in square brackets) is the slug; the braces contain the link text.

Page references have an advantage over hardcoded URLs: if you reorganize your site and a page's
output path changes, the reference still works because it's based on the slug, not the URL. Camp
resolves the current URL during each build.

@section[#:tag "nav-custom"]{Custom Navigation}

The @racket[prev-in] and @racket[next-in] functions provide navigation over arbitrary page lists,
not just the built-in collection and taxonomy orderings. They take a list of @racket[page-link]
structs and a slug, returning the adjacent page or @racket[#f]:

@codeblock{
(require camp)

;; Get all posts tagged "tutorial"
(define tutorials (get-taxonomy-pages "posts" "tags" "tutorial"))

;; Find neighbors of the current page within tutorials
(define prev (prev-in tutorials (context-slug ctxt)))
(define next (next-in tutorials (context-slug ctxt)))
}

This is useful when you need navigation that doesn't fit the standard patterns. You might create
a curated reading order that differs from chronological order, or navigate across collections
in a specific sequence.

The functions work with any list of page-links. You could filter, sort, or combine collections
however you need:

@codeblock{
(require racket/list)

;; Combine two collections and sort alphabetically by title
(define all-docs
  (sort (append (get-collection "guides")
                (get-collection "references"))
        string<?
        #:key page-link-title))

(define prev (prev-in all-docs current-slug))
}

@section[#:tag "nav-putting-together"]{Putting It Together}

Here's a more complete post template incorporating navigation features:

@codeblock|{
(define (render-post doc ctxt)
  (define title (or (meta-ref doc 'title) "Untitled"))
  (define date-str (meta-ref doc 'date))
  (define body (camp-doc->html-xexpr doc))
  (define tags (hash-ref (context-taxonomies ctxt) "tags" '()))
  (define p (prev ctxt))
  (define n (next ctxt))

  `(html
    (head
      (meta ((charset "utf-8")))
      (link ((rel "stylesheet") (href "/style.css")))
      (title ,title))
    (body
      (header
        (nav (a ((href "/")) "Home")))
      (article
        (h1 ,title)
        ,@(if date-str `((time ,date-str)) '())
        ,@body
        ,@(if (null? tags)
              '()
              `((p ((class "tags"))
                   "Tagged: "
                   ,@(add-between
                      (for/list ([tag tags])
                        `(a ((href ,(string-append "/tags/" tag "/")))
                            ,tag))
                      ", ")))))
      (nav ((class "post-nav"))
        ,(if p
             `(a ((href ,(page-link-url p))) "← " ,(page-link-title p))
             "")
        ,(if n
             `(a ((href ,(page-link-url n))) ,(page-link-title n) " →")
             ""))
      (footer
        (p "Powered by Camp")))))
}|

The tag links point to @filepath{/tags/emacs/} and similar---pages that don't exist yet. The next
tutorial shows how to create them using Camp's structural page language.
