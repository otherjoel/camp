#lang scribble/manual

@(require "doc-util.rkt"
          (for-label (except-in racket/base date date?)
                     camp
                     (except-in camp/page #%module-begin)
                     punct/doc))

@title[#:style 'quiet #:tag "mod-page"]{Structural Page Language}

@defmodulelang[camp/page]

The @hash-lang[] @racketmodname[camp/page] language provides an alternative to @hash-lang[]
@racketmodname[punct] for pages that are primarily structural or organizational---such as tag
listings, index pages, and archives---where CommonMark processing is not needed and full Racket
control over the output is desired.

Unlike Punct documents, @hash-lang[] @racketmodname[camp/page] documents do not pass through a
CommonMark parser. Instead, body expressions are wrapped in a thunk and evaluated at render time
when site information is available. This allows direct use of @racket[get-collection],
@racket[get-taxonomy-terms], @racket[get-taxonomy-pages], and other retrieval functions within the
page content.

@section[#:tag "ref-page-doc-structure"]{Document Structure}

A @hash-lang[] @racketmodname[camp/page] document consists of three sections:

@itemlist[#:style 'ordered
  @item{@bold{Module-level forms} (optional): @racket[require], @racket[provide], and @racket[define]
        forms that appear before any metadata. These are lifted to module level and evaluated at load
        time.}
  @item{@bold{Metadata}: Keyword-value pairs like @tt{#:title "Page Title"} that define page
        properties.}
  @item{@bold{Body}: All remaining forms, including any @racket[define] forms after metadata. These
        are wrapped in a thunk and evaluated at render time when @racket[current-site-info] is
        available.}]

@codeblock|{
#lang camp/page

(require racket/string)  ; lifted to module level

;; Helper defined before metadata - lifted to module level
(define (format-tag tag)
  (string-titlecase tag))

#:title "Browse by Tag"
#:slug "tags"

;; Everything after metadata is in the body thunk
(define tag-data (get-taxonomy-pages "blog" "tags"))

`(div ((class "content"))
   (h1 "Browse by Tag")
   (p "Posts organized by topic:")
   ,@(for/list ([tag (get-taxonomy-terms "blog" "tags")])
       `(section ((class "tag-section"))
          (h2 ,(format-tag tag))
          (ul ,@(for/list ([pg (hash-ref tag-data tag)])
                  `(li (a ((href ,(page-link-url pg)))
                          ,(page-link-title pg))))))))
}|

Metadata is specified using keyword-value pairs. The value following each keyword is read as a
Racket datum.

@tabular[#:sep @hspace[2]
         (list (list @bold{Keyword} @bold{Description})
               (list @tt{#:title} "Page title (used in templates and page index)")
               (list @tt{#:slug} "URL slug (defaults to filename if not specified)")
               (list @tt{#:date} "Publication date (for sorting and feed inclusion)")
               (list @tt{#:draft?} "If @racket[#t], excludes from feeds and navigation")
               (list @tt{#:output-path} "Override the collection's output path pattern"))]

Any other keywords are stored in the document metadata and accessible via @racket[meta-ref].

@section[#:tag "ref-page-bindings"]{Available Bindings}

The @hash-lang[] @racketmodname[camp/page] language provides all bindings from
@racketmodname[racket/base], plus:

@itemlist[
  @item{All exports from @racketmodname[camp]: @racket[get-collection], @racket[get-taxonomy-terms],
        @racket[get-taxonomy-pages], @racket[page-link-url], @racket[page-link-title],
        @racket[page-link-metas], @racket[prev], @racket[next], etc.}
  @item{All exports from @racketmodname[punct/doc]: the @racket[document] struct and related
        utilities.}]

Additional modules can be @racket[require]d as needed.

@section[#:tag "ref-page-render"]{Render Function Integration}

Documents written in @hash-lang[] @racketmodname[camp/page] produce a Punct-compatible
@racket[doc] binding with the body thunk stored in metadata. When render functions call
@racket[camp-doc->html-xexpr], it detects @hash-lang[] @racketmodname[camp/page] documents and
evaluates the thunk to produce the body content.

@defproc[(camp-page-doc? [doc any/c]) boolean?]{
Returns @racket[#t] if @racket[_doc] is a document produced by @hash-lang[]
@racketmodname[camp/page], @racket[#f] otherwise. Use this predicate to handle @hash-lang[]
@racketmodname[camp/page] documents differently from Punct documents in render functions.}

Render functions call @racket[camp-doc->html-xexpr] to convert the document. For @hash-lang[]
@racketmodname[camp/page] documents, this calls the body thunk; for Punct documents, it renders
the Punct content:

@codeblock|{
(require camp/page)  ; for camp-page-doc?

(define (render-page doc ctxt)
  (define body (camp-doc->html-xexpr doc))

  (if (camp-page-doc? doc)
      ;; camp/page: body already includes all markup
      (layout (meta-ref doc 'title) `((article ,@body)))
      ;; punct: add title heading
      (layout (meta-ref doc 'title)
              `((article (h1 ,(meta-ref doc 'title)) ,@body)))))
}|

@section[#:tag "ref-page-pagination"]{Pagination}

@declare-exporting[camp]

Camp provides pagination support for creating classical blog-style index pages that display
multiple posts per page with ``Older/Newer'' navigation. Pagination is implemented using the
@racket[paginate] form within @hash-lang[] @racketmodname[camp/page] documents.

@defproc[(paginate [collection-name string?]
                   [#:per-page per-page exact-positive-integer?]
                   [#:page-slug page-slug string? "page"]
                   [render-proc (-> (listof page-link?) pagination? any/c)])
         paginated-content?]{

Creates paginated output from a collection. When the build system encounters a
@racket[paginated-content] value in a @hash-lang[] @racketmodname[camp/page] body, it generates
multiple output files---one for each page of results.

The @racket[_collection-name] specifies which collection to paginate. The @racket[#:per-page]
argument controls how many items appear on each page.

The @racket[#:page-slug] argument controls the URL structure for pages 2 and beyond. With the
default @racket["page"], a page with @tt{#:output-path "/blog/"} produces:
@itemlist[
  @item{Page 1: @filepath{/blog/}}
  @item{Page 2: @filepath{/blog/page/2/}}
  @item{Page 3: @filepath{/blog/page/3/}}]

With @racket[#:page-slug "p"], the URLs become @filepath{/blog/p/2/}, @filepath{/blog/p/3/}, etc.

The @racket[_render-proc] is called once for each generated page. It receives two arguments:
@itemlist[
  @item{@racket[_items]: A @racket[(listof page-link?)] containing the items for the current page}
  @item{@racket[_pagination]: A @racket[pagination] struct with navigation information}]

The procedure should return an x-expression for the page body.

@codeblock|{
#lang camp/page

#:title "Blog"
#:output-path "/blog/"

(paginate "blog" #:per-page 10
  (λ (items pagination)
    `(main
      (h1 "Blog")
      ,@(for/list ([p items])
          `(article
            (h2 (a ([href ,(page-link-url p)]) ,(page-link-title p)))
            ,(camp-doc->html-xexpr (page-link-doc p))))
      ,(pagination-nav pagination))))
}|

Page titles are automatically modified for pages 2+: if the original title is ``Blog'', page 2
becomes ``Blog - Page 2'', page 3 becomes ``Blog - Page 3'', etc.

If the collection is empty, one page is still generated with an empty @racket[_items] list.}

@defstruct*[pagination ([page-num exact-positive-integer?]
                        [total-pages exact-positive-integer?]
                        [total-items natural?]
                        [base-url string?]
                        [current-url string?]
                        [prev-url (or/c string? #f)]
                        [next-url (or/c string? #f)])
                       #:transparent]{

A struct containing pagination context, passed to the render procedure in @racket[paginate].

@itemlist[
  @item{@racket[page-num]: The current page number (1-indexed)}
  @item{@racket[total-pages]: Total number of pages}
  @item{@racket[total-items]: Total number of items in the collection}
  @item{@racket[base-url]: URL of page 1 (e.g., @racket["/blog/"])}
  @item{@racket[current-url]: URL of the current page}
  @item{@racket[prev-url]: URL of the previous page, or @racket[#f] on page 1}
  @item{@racket[next-url]: URL of the next page, or @racket[#f] on the last page}]}

@defstruct*[paginated-content ([collection-name string?]
                               [per-page exact-positive-integer?]
                               [page-slug string?]
                               [render-proc procedure?])
                              #:transparent]{

Internal struct returned by @racket[paginate]. The build system detects this value and generates
multiple output files accordingly. You should not need to create this struct directly.}

@defproc[(pagination-nav [pagination pagination?]
                         [#:always-show? always-show? boolean? #f])
         any/c]{

Generates a navigation x-expression for pagination. Returns a @tt{<nav>} element with
previous/next links and a page indicator:

@verbatim|{
<nav class="pagination">
  <a href="/blog/" class="pagination-prev">← Newer</a>
  <span class="pagination-info">Page 2 of 5</span>
  <a href="/blog/page/3/" class="pagination-next">Older →</a>
</nav>
}|

By default, returns @racket['()] (empty list) when there is only one page, since navigation is
not needed. Pass @racket[#:always-show? #t] to display the navigation even for single-page
results.

The generated markup uses CSS classes for styling: @tt{pagination}, @tt{pagination-prev},
@tt{pagination-info}, and @tt{pagination-next}.}
