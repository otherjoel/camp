#lang scribble/manual

@(require "doc-util.rkt"
          scribble/example
          hash-view/scribble
          (for-label (except-in racket/base date date?)
                     camp
                     gregor))

@(define e (make-base-eval))
@(e '(require camp gregor))

@title[#:style 'quiet #:tag "mod-site"]{Site Configuration Language}

A Camp @deftech{site} is a single website, organized as a Racket package.

A site has one or more @deftech{collections}, which are named groups of pages with a sort order, a
mapping between source and output paths, and optional taxonomies for further organization.

@inline-note[#:type 'warning]{Note that the term @tech{collections} in Camp is different than
Racket's own concept of @secref["Library_Collections"
#:doc '(lib "scribblings/guide/guide.scrbl")].}

A @deftech{page} is a single source document.

A @deftech{feed} is an RSS or Atom feed which includes all @tech{pages} from a set of one or more
@tech{collections}. A @tech{site} may specify zero feeds, one feed, or multiple feeds.

@defmodulelang[camp/site]

The @hash-lang[] @racketmodname[camp/site] language provides a TOML-based configuration format for
defining Camp sites. Files written in this language are parsed and validated against the site
schema.

@section[#:tag "ref-site-required"]{Required Fields}

@tabular[#:sep @hspace[2]
         (list (list @bold{Field} @bold{Type} @bold{Description})
               (list @racket[title] "string" "Site title")
               (list @racket[url] "string" "Base URL (must be valid)")
               (list @racket[founded] "date" "Founding date for tag URI generation")
               (list @racket[authors] "array" "Authors in \"Name (email)\" format"))]

@section[#:tag "ref-site-optional"]{Optional Fields}

@tabular[#:sep @hspace[2]
         (list (list @bold{Field} @bold{Type} @bold{Default} @bold{Description})
               (list @racket[sources] "string" @racket[".md.rkt"] "Source file extension")
               (list @racket[static-folder] "string" @racket["static"] "Static assets directory")
               (list @racket[output-folder] "string" @racket["publish"] "Build output directory")
               (list @racket[deploy-script] "string" @racket[#f] "Deployment script path")
               (list @racket[default-render] "datum" @racket[#f] "Default render function"))]

@section[#:tag "ref-site-collections"]{Collections}

Each @tt{[[collections]]} entry defines a group of source documents:

@tabular[#:sep @hspace[2]
         (list (list @bold{Field} @bold{Type} @bold{Required} @bold{Description})
               (list @racket[name] "string" "yes" "Collection identifier")
               (list @racket[source] "string" "yes" "Glob pattern (must end with *)")
               (list @racket[output-paths] "string" "yes" "Output pattern with * and date codes")
               (list @racket[render-with] "datum" "no" "Render function specification")
               (list @racket[order] "string" "no" "\"ascending\" or \"descending\" (default)")
               (list @racket[sort-key] "string" "no" "Metadata key for sorting (default: \"date\")")
               (list @racket[taxonomies] "array" "no" "Taxonomy metadata keys"))]

@section[#:tag "ref-site-feeds"]{Feeds}

Each @tt{[[feeds]]} entry defines an RSS or Atom feed:

@tabular[#:sep @hspace[2]
         (list (list @bold{Field} @bold{Type} @bold{Description})
               (list @racket[filename] "string" "Output filename (.atom or .rss)")
               (list @racket[collections] "array" "Collection names to include")
               (list @racket[render-with] "datum" "Feed content render function"))]

The feed render function has the same signature as page render functions:

@codeblock|{
(define (feed-content doc ctxt)
  ;; doc: the Punct document
  ;; ctxt: same context as page render functions (slug, url, collection, etc.)
  ;; Returns x-expression for the feed entry body
  `(article ,@(document-body doc)
            (p (a ((href ,(context-url ctxt))) "Read more..."))))
}|

The @racket[ctxt] provides access to the page's canonical URL, enabling feed content to include
links back to the original page on your site.

@;------------------------------------------------
@section[#:tag "ref-path-mapping"]{Source/Output Path Mapping}

@declare-exporting[camp/main]

Camp uses path patterns to map source files to output locations. 

A @deftech{source path pattern} specifies where to find source documents within a collection (e.g.,
@racket["blog/*"]). An @deftech{output path pattern} specifies the URL structure for rendered pages,
with support for slug substitution and date-based paths (e.g., @racket["blog/[yyyy]/[MM]/*/"]).

An @tech{output path pattern} specifies the folder/file structure (and thus the URL) for rendered
pages, with support for slug substitution and date-based paths. In output path patterns:

@itemlist[#:style 'compact

@item{Any folder name consisting only of @litchar{*} will be replaced by the source’s @tech{slug}.}

@item{Any folder name consisting of valid CIDR syntax inside a pair of brackets @litchar{[]} will be
replaced by a string of the corresponding info from the source’s @tt{date} meta.}

@item{If the pattern ends in a trailing slash @litchar{/}, the output file will be named
@filepath{index.html}. Otherwise the output is the name of the pattern’s final element with an
added @filepath{.html} extension.}

]

@defproc[(source-path-pattern? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[_v] is a valid @tech{source path pattern}: a relative path string
that does not contain @tt{.} or @tt{..} components, and whose final element is @tt{*}.}

@defproc[(output-path-pattern? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[_v] is a valid @tech{output path pattern}: a relative path string
that does not contain @tt{.} or @tt{..} components, contains at least one @tt{*} element, and
where any bracketed patterns (e.g., @tt{[yyyy]}) are valid CLDR date format codes.}

@defproc[(format-output-path [pattern output-path-pattern?]
                              [slug string?]
                              [date (or/c date-provider? #f)])
         path?]{
Applies an @tech{output path pattern} to produce an output file path. The @racket[_slug] replaces
@tt{*} in the pattern, and @racket[_date] (if provided) is used for any bracketed date codes.

If the pattern contains date codes but @racket[_date] is @racket[#f], an error is raised.

@examples[#:eval e
(format-output-path "posts/*/" "hello-world" #f)
(format-output-path "blog/[yyyy]/[MM]/*/" "my-post" (date 2025 1 15))
]

}

@defproc[(file-extension? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[_v] is a valid file extension: a string or byte string starting with
@tt{.} and containing no directory separators.}

@defproc[(non-rkt-file-extension? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[_v] is a valid file extension other than @racket[".rkt"].}

@; =============================================================================
@section[#:tag "ref-site-config"]{Site Configuration API}

@declare-exporting[camp]

The following data types from @racketmodname[camp] underlie the site configuration language. They
are represented as @deftech{hash-views}: hash tables with struct-like accessor functions. For more
information on hash-views, see @other-doc['(lib "hash-view/hash-view.scrbl")].

@defhashview[site ([title string?]
                   [url valid-url-string?]
                   [founded date-provider?]
                   [authors (listof string?)]
                   [sources string? #:default ".md.rkt"]
                   [static-folder string? #:default "static"]
                   [output-folder string? #:default "publish"]
                   [collections (listof collection?)]
                   [racket-collection (or/c string? #f) #:default #f]
                   [deploy-script (or/c string? #f) #:default #f]
                   [default-render (or/c list? #f) #:default #f]
                   [feeds (listof feed-config?) #:default '()])]{

A @tech{hash-view} representing a @tech{site} configuration. A site is most commonly defined using
@hash-lang[] @racketmodname[camp/site].

Required fields are @racket[title], @racket[url], @racket[founded], @racket[authors], and
@racket[collections].

The @racket[racket-collection] field is set automatically by @racket[load-site] from the package's
@filepath{info.rkt}. It contains the Racket collection name (e.g., @racket["myblog"]) and is
@racket[#f] if the site is not installed as a package.}

@defhashview[collection ([name string?]
                          [source source-path-pattern?]
                          [output-paths output-path-pattern?]
                          [render-with (or/c list? #f)]
                          [order string? #:default "descending"]
                          [sort-key string? #:default "date"]
                          [taxonomies (listof string?)])]{

A @tech{hash-view} representing a @tech{collection} configuration. Collections are most commonly
defined as part of a @hash-lang[] @racketmodname[camp/site] configuration module.

Required fields are @racket[name], @racket[source], and @racket[output-paths].}

@defhashview[feed-config ([filename string?]
                           [collections (listof string?)]
                           [render-with list?])]{

A @tech{hash-view} representing a @tech{feed} configuration. Feed configurations are most commonly
defined as part of a @hash-lang[] @racketmodname[camp/site] configuration module. All fields are
required.}

@defproc[(load-site [mod-path (or/c path-string? module-path?
                                     (and/c hash? (λ (h) (hash-has-key? h 'path))))]) site?]{
Loads a site configuration from a @hash-lang[] @racketmodname[camp/site] module. The
@racket[_mod-path] can be:
@itemlist[
  @item{A filesystem path to a @filepath{site.rkt} file}
  @item{A module path like @racket['my-site/site]}
  @item{A hash containing a @racket['path] key (such as a book configuration returned by
        @racket[load-book])---the site is discovered from the package's @filepath{info.rkt}}]
Returns the parsed site configuration as a hash-view with an additional @racket['root] key
containing the absolute path to the site's directory.}

@defproc[(resolve-site-spec [spec (or/c path? string? symbol?)]) (or/c path? #f)]{
Resolves a site specification to a path. The @racket[_spec] can be:
@itemlist[
  @item{A @racket[path?]: Returns the path if it exists as a file, @racket[#f] otherwise.}
  @item{A @racket[string?]: If it exists as a file, returns it as a path. Otherwise, treats it as
        a collection name and searches installed packages.}
  @item{A @racket[symbol?]: Treats the symbol as a collection name and searches installed packages.}]

When searching by collection name, finds packages with a @racket['camp-site] field in their
@filepath{info.rkt} whose @racket['collection] name matches.

@codeblock|{
(resolve-site-spec "site.rkt")       ; path if file exists
(resolve-site-spec "myblog")         ; finds myblog package
(resolve-site-spec 'myblog)          ; same, with symbol
}|}

@defproc[(file-path->site-path [file-path path-string?]) path?]{
Discovers the site configuration path for a file within a Camp package. Uses
@racket[path->pkg+subpath] to find the package root, then reads the @racket['camp-site] field
from the package's @filepath{info.rkt}.

Raises an error if the file is not in a package, no @filepath{info.rkt} exists, or the
@racket['camp-site] field is not defined.}

@defproc[(load-book [file-path path-string?]) book?]{
Loads a book configuration from a @hash-lang[] @racketmodname[camp/book] module. Returns the
parsed book configuration as a hash-view with an additional @racket['path] key containing the
absolute path to the book file.

To load the associated site for a book:

@codeblock|{
(define mybook (load-book "path/to/my.book.rkt"))
(define mysite (load-site book))  ; discovers site from package info.rkt
}|}
