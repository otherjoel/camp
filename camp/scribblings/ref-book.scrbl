#lang scribble/manual

@(require "doc-util.rkt"
          hash-view/scribble
          (for-label racket/base
                     camp/book
                     racket/class
                     punct/render/typst))

@title[#:style 'quiet #:tag "mod-book"]{Book Configuration Language}

@defmodulelang[camp/book]

The @hash-lang[] @racketmodname[camp/book] language provides a TOML-based configuration format for
defining books that can be compiled to PDF via Typst. Book configurations reference site collections
and can filter pages by date range or taxonomy. All book metadata (title, authors, etc.) is handled
by your render function, not the configuration.

@section[#:tag "ref-book-required"]{Required Fields}

@tabular[#:sep @hspace[2]
         (list (list @bold{Field} @bold{Type} @bold{Description})
               (list @racket[render-with] "datum" "Render function @tt{(module-path identifier)}")
               (list @racket[output-folder] "string" "Output directory for .typ and .pdf files"))]

@section[#:tag "ref-book-optional"]{Optional Fields}

@tabular[#:sep @hspace[2]
         (list (list @bold{Field} @bold{Type} @bold{Description})
               (list @racket[includes] "array" "Files/directories to copy to output folder"))]

@section[#:tag "ref-book-parts"]{Parts}

Each @tt{[[parts]]} entry defines a section of the book:

@tabular[#:sep @hspace[2]
         (list (list @bold{Field} @bold{Type} @bold{Required} @bold{Description})
               (list @racket[name] "string" "yes" "Part name")
               (list @racket[pages] "array" "no" "Explicit page paths (relative to site root)")
               (list @racket[collections] "array" "no" "Collections to include")
               (list @racket[date-from] "date" "no" "Include pages on or after this date")
               (list @racket[date-to] "date" "no" "Include pages on or before this date")
               (list @racket[taxonomies] "table" "no" "Filter by taxonomy values"))]

The @racket[taxonomies] field is a table where keys are taxonomy names and values are either
a single string (exact match) or an array of strings (match any).

@section[#:tag "ref-book-example"]{Example}

@codeblock|{
#lang camp/book

render-with = "(mysite/render render-book)"
output-folder = "_output"
includes = ["template.typ", "fonts"]

[[parts]]
name = "Essays"
collections = ["posts"]
date-from = 2024-01-01
date-to = 2024-12-31
taxonomies = { category = "featured" }

[[parts]]
name = "Appendix"
pages = ["appendix-a.poly.pm"]
}|

The render function receives a list of gathered parts and returns a complete Typst document
as a string. See @secref{tutorial-book} for a complete example.

@; =============================================================================
@section[#:tag "ref-book-config"]{Book Configuration}

@declare-exporting[camp]

Book configurations define the structure for print/PDF output via Typst. All book metadata
(title, authors, etc.) is handled by your templates or render function, not the configuration.

For convenience, we use @deftech{chapter} to refer to a single source file that is included in a
book. A book @deftech{part} is a named collection of @tech{chapters}, built from a collection
(from the @racket[site] configuration) and optionally filtered by date ranges or taxonomy terms.

@defhashview[book ([render-with list?]
                   [output-folder string?]
                   [parts (listof book-part?)]
                   [path (or/c path? #f)]
                   [includes (listof string?)])]{

A @tech{hash-view} representing a book configuration. A book is most commonly defined using
@hash-lang[] @racketmodname[camp/book].

Required fields are @racket[render-with], @racket[output-folder], and @racket[parts]. The
@racket[path] field is set by @racket[load-book].

The @racket[render-with] field is a list of @racket[(module-path identifier)] specifying the
render function to call. The render function receives a list of @racket[part?] hashes and
returns a complete Typst document as a string.

The @racket[includes] field lists files or directories to copy to the output folder before
Typst compilation.}

@defhashview[book-part ([name string?]
                         [pages (listof string?)]
                         [collections (listof string?)]
                         [date-from (or/c date-provider? #f)]
                         [date-to (or/c date-provider? #f)]
                         [taxonomies hash?])]{

A @tech{hash-view} representing a book @tech{part} configuration. Book parts are most commonly
defined within a @hash-lang[] @racketmodname[camp/book] configuration module.

Only @racket[name] is required. Parts can include explicit @racket[pages] (paths relative to site
root), @racket[collections] to pull from, and filters (@racket[date-from], @racket[date-to],
@racket[taxonomies]) to select specific pages.}

@defhashview[part ([name string?]
                    [chapters (listof chapter?)])]{

A @tech{hash-view} containing a @deftech{gathered part}: the actual ordered list of
@tech{chapters} within a @tech{part} collected at build time. Passed to render functions.}

@defhashview[chapter ([slug string?]
                       [doc document?])]{

A @tech{hash-view} representing a gathered chapter. The @racket[doc] field contains the Punct
document; call @racket[camp-doc->typst] to convert it to Typst markup.}

@; =============================================================================
@section[#:tag "ref-typst-rendering"]{Typst Rendering}

@declare-exporting[camp]

Camp provides a Typst renderer for book publishing that extends Punct's Typst renderer with
cross-reference support.

@defclass[camp-typst-render% punct-typst-render% ()]{
Extends @racket[punct-typst-render%] with Camp-specific features:
@itemlist[
  @item{Automatic label attachment to leading H1 headings when a slug is provided}
  @item{Handling of @racket[page-ref] elements as Typst references}
  @item{Falls through to @racket[default-typst-tag] for @racket[term] and @racket[term-definition]}]

@defconstructor[([doc document?] [slug (or/c string? #f) #f])]{
Creates a renderer for @racket[_doc]. If @racket[_slug] is provided and the document starts with
a level-1 heading, the heading is rendered with a Typst label @tt{<slug>} attached, enabling
cross-references via @tt{@"@"slug}.}}

@defproc[(camp-doc->typst [doc document?] [#:slug slug (or/c string? #f) #f]) string?]{
Convenience function to render a Punct document to Typst markup. If @racket[_slug] is provided
and the document starts with a level-1 heading, a reference label is attached.

@codeblock|{
(camp-doc->typst doc)                    ; no label
(camp-doc->typst doc #:slug "intro")     ; attaches <intro> to leading H1
}|}

@defproc[(escape-typst-text [str string?]) string?]{
Escapes special Typst characters in text content. Re-exported from
@racketmodname[punct/render/typst].}

@defproc[(escape-typst-string [str string?]) string?]{
Escapes characters in quoted Typst string arguments (URLs, paths). Re-exported from
@racketmodname[punct/render/typst].}

@defproc[(default-typst-tag [tag symbol?]
                            [attrs (listof (list/c symbol? string?))]
                            [elems list?]) string?]{
Converts a custom element to a Typst function call. Re-exported from
@racketmodname[punct/render/typst].

@itemlist[
  @item{@racket[(term () "REST")] becomes @tt{#term[REST]}}
  @item{@racket[(term-definition ((name "rest")) "REST")] becomes
        @tt{#term_definition(name: "rest")[REST]}}
  @item{Hyphens in tag names are converted to underscores}]}

