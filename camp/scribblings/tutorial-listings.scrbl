#lang scribble/manual

@(require (for-label camp
                     (except-in camp/page #%module-begin)
                     punct/doc
                     punct/fetch
                     racket/base
                     racket/list
                     racket/string))

@title[#:tag "tutorial-listings"]{Tutorial: Listing and Index Pages}

Some pages exist primarily to organize and present other content: tag indices, paginated blog
listings, archives, and similar. These pages don't contain much prose---they're mostly generated
from site data. While you can build them with Punct and render functions, Camp provides an
alternative: @tt{#lang camp/page}, a language designed specifically for structural pages.

This tutorial covers @tt{#lang camp/page} and demonstrates building taxonomy indices, paginated
listings, and archive pages.

@section[#:tag "listings-problem"]{The Problem with Prose-First Pages}

Consider building a tag index---a page at @filepath{/tags/} listing all tags and the posts under
each one.

With Punct, you'd create a minimal source document:

@filebox["pages/tags.md.rkt"]{@codeblock{
#lang punct

---
title: Browse by Tag
slug: tags
---

Tags go here.
}}

Then your render function would do the real work, calling @racket[get-taxonomy-terms] and
@racket[get-taxonomy-pages] to build the listing. This works, but the page's structure lives in
the render module rather than in the page itself. If you have several such pages---one for tags,
one for series, one for archives---your render function becomes a dispatch table.

@tt{#lang camp/page} inverts this. The page itself contains the logic for building its content,
and the render function provides only the outer template.

@section[#:tag "listings-first-doc"]{Your First camp/page Document}

Here's a tag index written in @tt{#lang camp/page}:

@filebox["pages/tags.md.rkt"]{@codeblock|{
#lang camp/page

#:title "Browse by Tag"
#:slug "tags"

(define tag-pages (get-taxonomy-pages "blog" "tags"))

`((h1 "Browse by Tag")
  (div ((class "taxonomy-listing"))
       ,@(for/list ([tag (get-taxonomy-terms "blog" "tags")])
           `(section
              (h2 ,tag)
              (ul ,@(for/list ([pg (hash-ref tag-pages tag)])
                      `(li (a ((href ,(page-link-url pg)))
                              ,(page-link-title pg)))))))))
}|}

The structure differs from Punct in several ways. Metadata uses keyword syntax (@tt{#:title}
rather than YAML) since we're in a Racket context. The body is Racket code that evaluates to
X-expressions. And crucially, functions like @racket[get-taxonomy-terms] and
@racket[get-taxonomy-pages] work directly in the page---you don't need to call them from a render
function.

This direct access is the key feature. In Punct, document bodies are evaluated when the file is
loaded, before Camp has collected information about the site. @racket[get-collection] and
related functions only work during the render phase, when @racket[current-site-info] is populated.
A @tt{#lang camp/page} document's body is wrapped in a thunk that Camp evaluates at render time,
when all the site data is available.

Note the call to @racket[get-taxonomy-pages] with two arguments: it returns a hash mapping each
term to its list of @racket[page-link] structs. We call @racket[get-taxonomy-terms] separately
to iterate the tags in their natural order (the order they first appear in the collection). With
three arguments, @racket[get-taxonomy-pages] returns the page list for a single term directly.

@section[#:tag "listings-how-it-works"]{How camp/page Works}

A @tt{#lang camp/page} document has three sections: module-level forms, metadata, and body. The
language transforms them into a Punct-compatible document structure.

Module-level forms---@racket[require], @racket[provide], and @racket[define]---appear before any
metadata keywords. They're evaluated when the file loads, just like in any Racket module.

Metadata keywords (@tt{#:title}, @tt{#:slug}, @tt{#:date}, etc.) establish the document's
properties. Any keyword can be used; standard ones like @tt{#:title} have conventional meanings,
but you can add arbitrary metadata the same way.

Everything after the last metadata keyword is the body. It's wrapped in a thunk (a zero-argument
procedure) and stored in the document's metadata. When Camp renders the page, it detects this
structure and calls the thunk to produce the body content.

The result is that you write pages in a direct, procedural style, but they integrate seamlessly
with Camp's render pipeline.

@section[#:tag "listings-render-integration"]{Integrating with Render Functions}

Pages written in @tt{#lang camp/page} still pass through your render function. The render function
calls @racket[camp-doc->html-xexpr] to get the page body, just as it does for Punct documents.
For @tt{#lang camp/page} documents, this evaluates the body thunk; for Punct documents, it
renders the Punct content through Camp's HTML renderer.

Your render function can use the @racket[camp-page-doc?] predicate to handle them differently:

@codeblock|{
(require camp
         camp/page    ; for camp-page-doc?
         punct/fetch)

(define (render-page doc ctxt)
  (define title (or (meta-ref doc 'title) "Untitled"))
  (define body (camp-doc->html-xexpr doc))

  (layout title
    (if (camp-page-doc? doc)
        ;; camp/page: body already includes all markup
        `((article ,@body))
        ;; punct: add a title heading
        `((article (h1 ,title) ,@body)))))
}|

This lets you adjust rendering---perhaps Punct documents get an automatic @tt{<h1>} from the
title, while @tt{#lang camp/page} documents control their own headings.

@section[#:tag "listings-paginated"]{Paginated Listings}

A common use of @tt{#lang camp/page} is a blog index: a page showing recent posts with
Older/Newer navigation. Camp's @racket[paginate] function handles this, generating a separate
output file for each page of results:

@filebox["pages/blog.md.rkt"]{@codeblock|{
#lang camp/page

#:title "Blog"
#:output-path "/blog/"

(paginate "blog" #:per-page 10
  (λ (items pagination)
    `(main
       (h1 "Blog")
       ,@(for/list ([p items])
           `(article
              (h2 (a ((href ,(page-link-url p)))
                     ,(page-link-title p)))))
       ,(pagination-nav pagination))))
}|}

When the body evaluates to a @racket[paginated-content] value, the build system generates
multiple output files: @filepath{/blog/} for page 1, @filepath{/blog/page/2/} for page 2, and
so on. The render procedure receives two arguments: the @racket[page-link] items for the current
page, and a @racket[pagination] struct with navigation URLs. @racket[pagination-nav] produces a
ready-made navigation element with Older/Newer links, or you can use the @racket[pagination]
struct accessors to build your own.

For richer entries---showing post content rather than just titles---use @racket[page-link-doc]
to retrieve the full document for a page link:

@codeblock|{
(paginate "blog" #:per-page 5
  (λ (items pagination)
    `(main
       (h1 "Blog")
       ,@(for/list ([p items])
           `(article
              (h2 (a ((href ,(page-link-url p)))
                     ,(page-link-title p)))
              ,@(camp-doc->html-xexpr (page-link-doc p))))
       ,(pagination-nav pagination))))
}|

@section[#:tag "listings-archive-pages"]{Archive Pages}

Archive pages---yearly or monthly indices of posts---follow a similar pattern. Here's a yearly
archive that groups posts by the year in their date metadata:

@filebox["pages/archive.md.rkt"]{@codeblock|{
#lang camp/page

(require racket/list)

#:title "Archive"
#:slug "archive"

(define posts (get-collection "blog"))

(define (post-year p)
  (define date (hash-ref (page-link-metas p) 'date #f))
  (if date (~d "yyyy" date) "Undated"))

(define by-year (group-by post-year posts))

`(div ((class "archive"))
   (h1 "Archive")
   ,@(for/list ([group by-year])
       (define year (post-year (first group)))
       `(section
          (h2 ,year)
          (ul
            ,@(for/list ([p group])
                `(li
                   (a ((href ,(page-link-url p)))
                      ,(page-link-title p))))))))
}|}

The @racket[group-by] function (from @racketmodname[racket/list]) collects posts sharing the
same year. The @racket[~d] function formats dates regardless of whether the value is a string
or a date object. Since the collection is already sorted by date descending, the groups appear
newest-first.

@section[#:tag "listings-when-to-use"]{When to Use Each Approach}

Use Punct (@tt{#lang punct}) for prose-heavy content: blog posts, articles, documentation pages,
and anything where you're primarily writing text with occasional embedded logic. Punct's Markdown
support makes writing comfortable, and the CommonMark parser handles formatting details.

Use @tt{#lang camp/page} for structural content: taxonomy indices, paginated blog listings,
archives, and pages that are mostly generated from site data. Writing these in pure Racket avoids
the awkward dance of minimal Punct documents with heavy render functions, and gives you direct
access to @racket[get-collection], @racket[get-taxonomy-pages], and @racket[paginate].

The two approaches complement each other. A typical site has many Punct documents (posts, regular
pages) and a handful of @tt{#lang camp/page} documents (the home page, tag index, archive,
paginated blog listing). The render function provides consistent templating across both.
