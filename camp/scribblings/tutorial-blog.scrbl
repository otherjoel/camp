#lang scribble/manual

@(require "doc-util.rkt"
          (for-label camp
                     camp/xref
                     camp/build
                     punct/doc
                     racket/base
                     racket/list))

@title[#:tag "tutorial-blog"]{Building a Camp Site}

Here I’ll explain how to build a website in Camp from scratch. As explained in @secref["quickstart"],
you can use @tt{raco camp} commands to jump-start much of this, but a no-frills explanation is
good for you and builds character.

@inline-note[#:type 'warning]{I am not going to hold your hand a lot here. Lucky for you, Racket’s 
documentation system is second to none. There are hyperlinks everywhere, including throughout any
code samples you see. Follow them.}

@section[#:tag "blog-project-setup"]{Clear a spot to work in}

Start by creating a directory for your site. We'll call it @filepath{myblog}:

@terminal{
@:>{mkdir myblog}
@:>{cd myblog}}

Create a directory for your posts and another for your static assets:

@terminal{
@:>{mkdir posts}
@:>{mkdir static}}

Now you have your folder structure.

@section[#:tag "blog-pkg-info"]{Installing as a Racket package}

@margin-note{This part isn’t strictly necessary. But Camp makes use of conveniences provided by the
Racket package system, so it’s simpler just to do it now.}

A Camp site is a pile of code that produces a bunch of web pages. You can (and should) install that
pile of code as a Racket package.

Create @filepath{info.rkt} with the following content:

@filebox["info.rkt"]{@codeblock{
#lang info
(define collection "myblog")
(define deps '("base" "camp"))
(define camp-site "site.rkt")
}}

The @tt{collection} line gives your site a name that Racket can use to find its modules. The
@tt{deps} line declares that your site depends on Camp (and @racket["base"], which is Racket's
core library). The @tt{camp-site} line tells Camp which file contains your site configuration.

Now install your site as a local package so Racket can find it:

@terminal{@:>{raco pkg install}}

Run this command from inside your @filepath{myblog} directory. You only need to do this once.

@section[#:tag "blog-site-config"]{Configure your site}

Create a new file in your project’s root folder:

@filebox["site.rkt"]{@codeblock|{
#lang camp/site

title = "My Blog"
url = "https://example.com"
founded = 2026-01-01
authors = ["Your Name (you@example.com)"]

[[collections]]
name = "posts"
source = "posts/*"
output-paths = "posts/[yyyy]/[MM]/*/"
render-with = "(myblog/render render-post)"
order = "descending"
sort-key = "date"
}|}

The @hash-lang[] @racketmodname[camp/site] language uses @hyperlink["https://toml.io/en/"]{TOML
syntax}.

Camp always looks for a @filepath{site.rkt} module before it does anything. So now Camp knows how your
site is organized and how to publish it. We just need to add the stuff that your @filepath{site.rkt}
refers to.

@section{First post}

Your @filepath{site.rkt} specified a single @tech{collection} named @racket{posts}, whose source
files are located in the @filepath{posts/} subfolder. So let's put a post in that folder:

@margin-note{The Punct language is essentially a Markdown environment which allows escaping to Racket 
with the @litchar{•} character, and which compiles to a format-independent AST. Read 
@hyperlink["https://joeldueck.com/what-about/punct/Writing_Punct.html"]{Writing Punct} for more about
its syntax.}

@filebox["posts/hello-world.md.rkt"]{@codeblock|{
#lang punct 

---
title: Hello, World
date: 2026-01-15
---

# Welcome

This is my first blog post. I'm building a site with *Camp*,
a static site generator for Racket.

Here's something Markdown can't normally do: today's year is
•(date-year (seconds->date (* 0.001 (current-inexact-milliseconds)))).
}|}

@section[#:tag "blog-render-function"]{Add a render function}

In the @tt{[[collections]]} section, your @filepath{site.rkt} included a @tt{render-with} directive.
This points Camp to the module, and the function within that module, that must be used to render
source documents in that collection to HTML.

@;{A @emph{render function} receives a parsed document and a context containing pre-rendered content and
navigation helpers, then returns an X-expression representing the complete HTML page. X-expressions
are Racket's way of representing structured markup as nested lists; they look odd at first but
become natural with practice.}

@margin-note{See @secref["module-paths" #:doc '(lib "scribblings/guide/guide.scrbl")] in the Racket
Guide}

In your case, you told Camp the render function for the @racket{posts} collection would be the
@racketidfont{render-post} function provided by the @racketmodfont{myblog/render} module. So let’s
create that file. The site is already installed as a Racket package that uses the
@racketmodfont{myblog} collection name, so a @filepath{render.rkt} located in our root folder will
answer to the @racketmodfont{myblog/render} module path:

@filebox["render.rkt"]{@codeblock{
#lang racket/base

(require camp
         punct/fetch)

(provide render-post)

(define (render-post doc ctxt)
  (define title (get-meta doc 'title "Untitled"))

  `(html
    (head
      (meta ((charset "utf-8")))
      (link ((rel "stylesheet") (href "/style.css")))
      (title ,title))
    (body
      (header
        (h1 ,title))
    (article
    ,@"@"(camp-doc->html-xexpr doc))
      (footer
        (p "Powered by Camp")))))
}}

Your render function must take a Punct @racket[document] and a render @racket[context]

@section[#:tag "blog-static-assets"]{Adding Static Assets}

Most sites need stylesheets, images, or JavaScript files. Camp copies everything in your static
folder to the output directory unchanged. Create a basic 
stylesheet in your @filepath{static/} subfolder:

@filebox["static/style.css"]{@verbatim|{
body {
    max-width: 40rem;
    margin: 2rem auto;
    padding: 0 1rem;
    font-family: system-ui, sans-serif;
    line-height: 1.5;
}

header { margin-bottom: 2rem; }
footer { margin-top: 3rem; color: #666; }
}|}

@section[#:tag "blog-building"]{Building Your Site}

With all of this in place, you can now build your site:

@terminal{
@:>{raco camp build}
 ● Collect   1 page in 1 collection                    63ms
 ● Build     1 page                                    20ms
 ● Static    1 file

 ✓ Done in 149ms}

You’ll see the site’s files and folders in a new @filepath{publish/} subfolder.

To preview the site:

@terminal{
@:>{raco camp serve}
  Serving /Users/joel/code/myblog/publish
  URL     http://localhost:8000
  Watching for changes...
  Press Ctrl+C to stop}

Browse to @url{http://localhost:8000/posts/2026/01/hello-world/} to see the post we created.

@section[#:tag "blog-home-page"]{Add a home page}

Add a second @tech{collection} to @filepath{site.rkt}:

@filebox["site.rkt"]{@codeblock|{
#lang camp/site

title = "My Blog"
url = "https://example.com"
founded = 2026-01-01
authors = ["Your Name (you@example.com)"]

[[collections]]
name = "posts"
source = "posts/*"
output-paths = "posts/[yyyy]/[MM]/*/"
render-with = "(myblog/render render-post)"
order = "descending"
sort-key = "date"

[[collections]]
name = "pages"
source = "pages/*"
output-paths = "*/"
render-with = "(myblog/render render-page)"
}|}

Create a @filepath{pages/} subfolder and an index page:

@filebox["pages/index.md.rkt"]{@codeblock|{
#lang punct

---
title: Home
output-path: /
---

# Welcome to My Blog
}|}

The @tt{output-path} metadata overrides the collection's default output path for this page.

Add @racket[render-page] to @filepath{render.rkt}:

@filebox["render.rkt"]{@codeblock{
#lang racket/base

(require camp
         punct/fetch
         racket/list)

(provide render-post
         render-page)

(define (render-post doc ctxt)
  (define title (get-meta doc 'title "Untitled"))

  `(html
    (head
      (meta ((charset "utf-8")))
      (link ((rel "stylesheet") (href "/style.css")))
      (title ,title))
    (body
      (header
        (nav (a ((href "/")) "Home"))
        (h1 ,title))
    (article
    ,@"@"(camp-doc->html-xexpr doc))
      (footer
        (p "Powered by Camp")))))

(define (render-page doc ctxt)
  (define title (get-meta doc 'title "Untitled"))
  (define posts (get-collection "posts" #:limit 5))

  `(html
    (head
      (meta ((charset "utf-8")))
      (link ((rel "stylesheet") (href "/style.css")))
      (title ,title))
    (body
      (header
        (h1 ,title))
      (main
        ,@"@"(camp-doc->html-xexpr doc)
        (h2 "Recent Posts")
        (ul
          ,@"@"(for/list ([p posts])
               `(li (a ((href ,(page-link-url p)))
                       ,(page-link-title p))))))
      (footer
        (p "Powered by Camp")))))
}}

@section[#:tag "blog-main-module"]{Add a meta-language}

You can make Camp's functions available inside source documents by creating a @filepath{main.rkt}
that Punct can use as a @seclink["hash-languages" #:doc '(lib "scribblings/guide/guide.scrbl")]{meta-language}:

@filebox["main.rkt"]{@codeblock{
#lang racket/base

(require camp
         camp/xref)

(provide (all-from-out camp)
         (all-from-out camp/xref))
}}

Now source files that use @tt{#lang punct myblog} can call @racket[get-collection], @racket[~d],
@racket[defterm], etc. directly.

@section[#:tag "blog-feed"]{Add a feed}

Append a @tt{[[feeds]]} section to @filepath{site.rkt}:

@filebox["site.rkt (append)"]{@codeblock|{
[[feeds]]
filename = "feed.atom"
collections = ["posts"]
render-with = "(myblog/render feed-content)"
}|}

The extension determines the format: @filepath{.atom} → Atom, @filepath{.rss} → RSS.

Add the feed renderer to @filepath{render.rkt}:

@filebox["render.rkt (addition)"]{@codeblock{
(provide render-post
         render-page
         feed-content)

(define (feed-content doc)
  (camp-doc->html-xexpr doc))
}}

Posts missing a @tt{date} or marked @tt{draft: true} are excluded from feeds.

@section[#:tag "blog-whats-next"]{What's next}

@secref["tutorial-navigation"] covers Camp's cross-reference system.
@secref["tutorial-listings"] introduces a language for aggregate pages like archives and tag indices.
