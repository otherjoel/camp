#lang scribble/manual

@(require (for-label camp
                     punct/doc
                     racket/base))

@title[#:tag "tutorial-book"]{Tutorial: Creating a Book from Your Blog}

This tutorial shows how to compile your blog posts into a print-ready PDF book using Camp's
Typst integration. We'll create a book configuration, write a render function, and build
a PDF containing selected posts from your site.

This tutorial assumes you have a working Camp site with a blog collection. If you don't have one
yet, work through the @secref{tutorial-blog} tutorial first.

@section[#:tag "book-overview"]{How Book Publishing Works}

Camp's book publishing is intentionally minimal. Camp handles:

@itemlist[
  @item{Selecting pages from your site's collections based on filters you define}
  @item{Organizing them into parts and chapters}
  @item{Passing them to your render function}
  @item{Writing the output to a @tt{.typ} file and calling @tt{typst compile}}]

Everything else---the book's metadata, layout, typography, title page, table of contents---is
your responsibility. You control all of this through your render function and Typst template.
This approach gives you complete flexibility while keeping Camp's book system simple.

Before starting, make sure you have @hyperlink["https://typst.app"]{Typst} installed. You can
install it via your package manager (@tt{brew install typst} on macOS) or download it from
the Typst website.

@section[#:tag "book-config"]{Creating the Book Configuration}

Book configurations use the @tt{#lang camp/book} language. Create a file called
@filepath{essays.book.rkt} in your site's root directory:

@filebox["essays.book.rkt"]{@codeblock|{
#lang camp/book

render-with = "(mysite/render render-book)"
output-folder = "_output"
includes = ["template.typ"]

[[parts]]
name = "Essays"
collections = ["posts"]
date-from = 2024-01-01
date-to = 2024-12-31
}|}

The configuration has three required fields:

@itemlist[
  @item{@bold{render-with}: A readable datum specifying the module path and identifier of your
        render function. The function receives a list of parts and returns a complete Typst
        document as a string.}
  @item{@bold{output-folder}: Where to write the @tt{.typ} and @tt{.pdf} files.}
  @item{@bold{parts}: At least one part, each with a name and content sources.}]

The optional @bold{includes} field lists files or directories to copy to the output folder
before Typst compilation. Use this for templates, fonts, or images referenced by your Typst code.

@section[#:tag "book-render"]{Writing a Render Function}

Your render function receives a list of parts. Each part is a hash with @racket['name] (string)
and @racket['chapters] (list). Each chapter is a hash with @racket['slug] (string) and
@racket['doc] (a Punct document).

Create @filepath{render.rkt} (or add to your existing one):

@filebox["render.rkt"]{@codeblock|{
#lang racket/base

(require camp
         punct/doc
         racket/string)

(provide render-book)

(define (render-book parts)
  (string-append
   ;; Preamble: import template and configure the book
   #<<TYPST
#import "template.typ": book

#show: book.with(
  title: "Selected Essays",
  authors: ("Your Name",),
  year: 2024,
)

TYPST
   ;; Render each part
   (string-append*
    (for/list ([part (in-list parts)])
      (render-part part)))))

(define (render-part part)
  (define name (hash-ref part 'name))
  (define chapters (hash-ref part 'chapters))
  (string-append
   (format "= ~a\n\n" name)
   (string-append*
    (for/list ([ch (in-list chapters)])
      (render-chapter ch)))))

(define (render-chapter ch)
  (define slug (hash-ref ch 'slug))
  (define doc (hash-ref ch 'doc))
  (define title (or (meta-ref doc 'title) slug))
  (string-append
   ;; Chapter heading with label for cross-references
   (format "== ~a <~a>\n\n" title slug)
   ;; Render the document body to Typst
   (camp-doc->typst doc)
   "\n\n"))
}|}

The key function is @racket[camp-doc->typst], which converts a Punct document to Typst markup.
Your render function controls everything else: the preamble, part headings, chapter headings,
and how content is assembled.

@section[#:tag "book-template"]{Writing a Typst Template}

The template controls your book's visual design. Create @filepath{template.typ}:

@filebox["template.typ"]{@verbatim|{
#let book(
  title: none,
  authors: (),
  year: none,
  body,
) = {
  set document(title: title, author: authors.join(", "))
  set text(font: "Linux Libertine", size: 11pt)
  set page(paper: "us-trade", margin: (inside: 0.875in, outside: 0.625in))
  set par(leading: 0.65em, first-line-indent: 1.5em, justify: true)

  // Title page
  align(center + horizon)[
    #text(size: 24pt, weight: "bold")[#title]
    #v(1em)
    #text(size: 14pt)[#authors.join(" and ")]
    #v(2em)
    #text(size: 12pt)[#year]
  ]

  pagebreak()
  outline(title: "Contents")
  pagebreak()

  // Chapter headings
  show heading.where(level: 1): it => {
    pagebreak(weak: true)
    v(2em)
    text(size: 18pt, weight: "bold", it.body)
    v(1em)
  }

  show heading.where(level: 2): it => {
    v(1.5em)
    text(size: 14pt, weight: "bold", it.body)
    v(0.5em)
  }

  body
}
}|}

Your render function imports this template and applies it using @tt{#show: book.with(...)}.
All book metadata (title, authors, etc.) flows through the render function into the template---not
through Camp's configuration.

@section[#:tag "book-parts"]{Organizing Your Book}

Parts can include content from collections (filtered by date and taxonomy) or explicit pages:

@verbatim|{
[[parts]]
name = "2024 Essays"
collections = ["posts"]
date-from = 2024-01-01
date-to = 2024-12-31
taxonomies = { series = "essays" }

[[parts]]
name = "Technical Articles"
collections = ["posts"]
taxonomies = { tags = ["programming", "racket"] }

[[parts]]
name = "Appendix"
pages = ["appendix-a.poly.pm", "appendix-b.poly.pm"]
}|

@subsection[#:tag "book-filters"]{Filtering Content}

Parts that pull from collections can filter pages by:

@itemlist[
  @item{@bold{Date range}: @tt{date-from} and @tt{date-to} select posts within a time period}
  @item{@bold{Taxonomies}: The @tt{taxonomies} table filters by metadata values. A string value
        requires an exact match; an array matches if any value is present.}]

For example, to include posts tagged with either "programming" or "racket":

@verbatim|{
taxonomies = { tags = ["programming", "racket"] }
}|

@section[#:tag "book-building"]{Building the Book}

Build your book with:

@commandline{raco camp print essays.book.rkt}

This command:

@itemlist[#:style 'ordered
  @item{Loads your site configuration}
  @item{Gathers and filters pages into parts and chapters}
  @item{Copies includes to the output folder}
  @item{Calls your render function}
  @item{Writes the result to a @tt{.typ} file}
  @item{Runs @tt{typst compile} to produce a PDF}]

The output files use the book file's name (e.g., @filepath{_output/essays.typ} and
@filepath{_output/essays.pdf}).

@section[#:tag "book-custom-elements"]{Handling Custom Elements}

If your blog posts use custom elements, you can handle them in two ways:

@bold{In your render function}: Pass a custom render class to @racket[camp-doc->typst] that
handles your elements specially.

@bold{In your Typst template}: Define Typst functions for elements like @tt{term} and
@tt{page-ref}:

@verbatim|{
#let term(body) = emph(body)
#let term_definition(body, name: none) = strong(body)
}|

For page references, the Typst output includes labels (e.g., @tt{<my-post>}) that you can
reference with @tt{@"@"my-post} or @tt{#link(<my-post>)[custom text]}.

@section[#:tag "book-tips"]{Tips for Book Production}

Blog posts often need adjustments for print:

@itemlist[
  @item{@bold{Links}: External URLs don't work in print. Consider footnotes with URLs or
        styling links to show the URL inline.}
  @item{@bold{Images}: Web images may need higher resolution for print.}
  @item{@bold{Length}: Very short posts may feel sparse as chapters. Consider grouping
        related short posts.}]

See the @hyperlink["https://typst.app/docs"]{Typst documentation} for details on advanced
formatting, running headers, page numbers, and other print production features.
