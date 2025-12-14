#lang scribble/manual

@(require (for-label camp
                     camp/xref
                     camp/build
                     camp/serve
                     camp/log
                     (except-in gregor date date?)
                     punct/doc
                     racket/base
                     racket/contract))

@title[#:tag "reference"]{Library Reference}

@local-table-of-contents[]

@; =============================================================================
@section[#:tag "mod-camp"]{Main Interface}

@defmodule[camp]

The @racketmodname[camp] module is the primary interface for working with Camp sites. It provides
access to site configuration, page collections, and navigation utilities.

@subsection{Site Configuration}

Site configuration and collection definitions are represented as hash-views: hash tables with
struct-like accessor functions.

@defproc[(site? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[_v] is a valid site configuration hash (containing all required
fields), @racket[#f] otherwise.}

@deftogether[(@defproc[(site-title [s site?]) string?]
              @defproc[(site-url [s site?]) string?]
              @defproc[(site-founded [s site?]) date-provider?]
              @defproc[(site-authors [s site?]) (listof string?)]
              @defproc[(site-sources [s site?]) string?]
              @defproc[(site-static-folder [s site?]) string?]
              @defproc[(site-output-folder [s site?]) string?]
              @defproc[(site-collections [s site?]) (listof collection?)]
              @defproc[(site-deploy-script [s site?]) (or/c string? #f)]
              @defproc[(site-default-render [s site?]) (or/c list? #f)]
              @defproc[(site-element-fallback [s site?]) (or/c string? #f)]
              @defproc[(site-feeds [s site?]) (listof feed-config?)])]{
Accessors for site configuration fields. Required fields are @racket[title], @racket[url],
@racket[founded], @racket[authors], and @racket[collections]. The @racket[sources] field defaults to
@racket[".md.rkt"], @racket[static-folder] to @racket["static"], and @racket[output-folder] to
@racket["publish"].}

@defproc[(collection? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[_v] is a valid collection definition hash, @racket[#f] otherwise.}

@deftogether[(@defproc[(collection-name [c collection?]) string?]
              @defproc[(collection-source [c collection?]) string?]
              @defproc[(collection-output-paths [c collection?]) string?]
              @defproc[(collection-render-with [c collection?]) (or/c list? #f)]
              @defproc[(collection-order [c collection?]) string?]
              @defproc[(collection-sort-key [c collection?]) string?]
              @defproc[(collection-taxonomies [c collection?]) (listof string?)])]{
Accessors for collection fields. The @racket[name], @racket[source], and @racket[output-paths] fields
are required. The @racket[order] defaults to @racket["descending"], @racket[sort-key] to
@racket["date"], and @racket[taxonomies] to @racket['()].}

@defproc[(feed-config? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[_v] is a valid feed configuration hash, @racket[#f] otherwise.}

@deftogether[(@defproc[(feed-config-filename [f feed-config?]) string?]
              @defproc[(feed-config-collections [f feed-config?]) (listof string?)]
              @defproc[(feed-config-render-with [f feed-config?]) list?])]{
Accessors for feed configuration fields. All fields are required.}

@subsection{Site Loading}

@defproc[(load-site [mod-path (or/c path-string? module-path?)]) site?]{
Loads a site configuration from a @tt{#lang camp/site} module. The @racket[_mod-path] can be
either a filesystem path or a module path. Returns the parsed site configuration as a hash-view.}

@subsection{Internal Structures}

@defstruct[page ([source-path path?]
                 [output-path path?]
                 [doc any/c]
                 [slug string?]
                 [collection-name string?])
                #:transparent]{
Represents a page during the build process. Contains the source file path, computed output path,
the Punct document, URL slug, and the name of the collection it belongs to.}

@defstruct[page-link ([url string?]
                      [title string?]
                      [metas hash?])
                     #:transparent]{
A lightweight reference to a page, used for navigation and collection retrieval. Contains the page's
URL, title, and full metadata hash.}

@defstruct[site-info ([pages (listof page?)]
                      [term-index hash?]
                      [page-index hash?]
                      [taxonomy-index hash?])
                     #:transparent]{
Collected information about a site, built during the collect pass. Contains all processed pages,
an index mapping term names to URLs, an index mapping slugs to page-links, and a nested index for
taxonomy lookups.}

@subsection{Context}

The render context is passed to render functions and provides pre-rendered content along with
navigation helpers.

@defproc[(context? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[_v] is a valid render context hash, @racket[#f] otherwise.}

@deftogether[(@defproc[(context-body [ctx context?]) (listof any/c)]
              @defproc[(context-slug [ctx context?]) string?]
              @defproc[(context-collection [ctx context?]) string?]
              @defproc[(context-prev [ctx context?]) procedure?]
              @defproc[(context-next [ctx context?]) procedure?]
              @defproc[(context-taxonomies [ctx context?]) hash?])]{
Accessors for render context fields. The @racket[body] contains pre-rendered HTML x-expressions with
cross-references resolved. The @racket[prev] and @racket[next] fields are procedures that accept
zero to two arguments for navigation (see specification for calling conventions). The
@racket[taxonomies] field maps taxonomy keys to lists of normalized values for the current page.}

@subsection{Collection Retrieval}

@defproc[(get-collection [name string?]
                         [#:limit limit (or/c #f exact-positive-integer?) #f]
                         [#:full-docs? full-docs? boolean? #f])
         list?]{
Retrieves pages from a named collection. Returns a list of @racket[page-link] structs by default,
or full Punct documents if @racket[_full-docs?] is @racket[#t]. The optional @racket[_limit]
restricts the number of results returned.}

@subsection{Taxonomy Functions}

@defproc[(get-taxonomy-terms [collection-name string?]
                             [taxonomy-key string?])
         (listof string?)]{
Returns all distinct values for a taxonomy within a collection, ordered by first appearance in the
collection's sort order.}

@defproc[(get-taxonomy-pages [collection-name string?]
                             [taxonomy-key string?]
                             [term (or/c string? #f) #f])
         (or/c (listof page-link?) hash?)]{
With two arguments, returns a hash mapping each taxonomy term to its list of page-links.
With three arguments (including a specific @racket[_term]), returns the list of page-links
for that term.}

@subsection{Page Navigation}

@defproc[(prev-in [pages (listof page-link?)]
                  [current-slug string?])
         (or/c page-link? #f)]{
Finds the previous page in an ordered list, given the current page's slug. Returns @racket[#f] if
there is no previous page.}

@defproc[(next-in [pages (listof page-link?)]
                  [current-slug string?])
         (or/c page-link? #f)]{
Finds the next page in an ordered list, given the current page's slug. Returns @racket[#f] if there
is no next page.}

@; =============================================================================
@section[#:tag "mod-site"]{Site Configuration Language}

@defmodulelang[camp/site]

The @tt{#lang camp/site} language provides a TOML-based configuration format for defining Camp
sites. Files written in this language are parsed and validated against the site schema.

@subsection{Required Fields}

@tabular[#:sep @hspace[2]
         (list (list @bold{Field} @bold{Type} @bold{Description})
               (list @racket[title] "string" "Site title")
               (list @racket[url] "string" "Base URL (must be valid)")
               (list @racket[founded] "date" "Founding date for tag URI generation")
               (list @racket[authors] "array" "Authors in \"Name (email)\" format"))]

@subsection{Optional Fields}

@tabular[#:sep @hspace[2]
         (list (list @bold{Field} @bold{Type} @bold{Default} @bold{Description})
               (list @racket[sources] "string" @racket[".md.rkt"] "Source file extension")
               (list @racket[static-folder] "string" @racket["static"] "Static assets directory")
               (list @racket[output-folder] "string" @racket["publish"] "Build output directory")
               (list @racket[deploy-script] "string" @racket[#f] "Deployment script path")
               (list @racket[default-render] "datum" @racket[#f] "Default render function")
               (list @racket[element-fallback] "string" @racket[#f] "Custom element renderer"))]

@subsection{Collections}

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

@subsection{Feeds}

Each @tt{[[feeds]]} entry defines an RSS or Atom feed:

@tabular[#:sep @hspace[2]
         (list (list @bold{Field} @bold{Type} @bold{Description})
               (list @racket[filename] "string" "Output filename (.atom or .rss)")
               (list @racket[collections] "array" "Collection names to include")
               (list @racket[render-with] "datum" "Feed content render function"))]

@subsection{Path Pattern Contracts}

@defproc[(source-path-pattern? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[_v] is a valid source path pattern: a relative path string that does
not contain @tt{.} or @tt{..} components, and whose final element is @tt{*}.}

@defproc[(output-path-pattern? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[_v] is a valid output path pattern: a relative path string that does
not contain @tt{.} or @tt{..} components, contains at least one @tt{*} element, and where any
bracketed patterns (e.g., @tt{[yyyy]}) are valid CLDR date format codes.}

@defproc[(file-extension? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[_v] is a valid file extension: a string or byte string starting with
@tt{.} and containing no directory separators.}

@defproc[(non-rkt-file-extension? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[_v] is a valid file extension other than @racket[".rkt"].}

@; =============================================================================
@section[#:tag "mod-xref"]{Cross-Reference System}

@defmodule[camp/xref]

The @racketmodname[camp/xref] module provides functions for defining terms and creating
cross-references within Punct source documents.

@defproc[(defterm [name string?] [content any/c] ...) list?]{
Defines a term in a Punct document. Produces a @racket[term-definition] x-expression that will be
rendered as a @tt{<dfn>} element with an anchor. The term is also indexed for cross-reference
resolution.

In Punct source: @tt{@"•"defterm["REST"]@"{Representational State Transfer@"}"}}

@defproc[(term [name string?]) list?]{
References a defined term. Produces a @racket[term] x-expression that will be resolved to a link
during rendering.

In Punct source: @tt{@"•"term@"{REST@"}"}}

@defproc[(page-ref [slug string?] [content any/c] ...) list?]{
References another page by its slug. Spaces in the slug are normalized to hyphens. If no content is
provided, the page's title is used as the link text.

In Punct source: @tt{@"•"page-ref@"{my-slug@"}"} or @tt{@"•"page-ref["other-page"]@"{link text@"}"}

Produces a @racket[page-ref] x-expression that will be resolved to a link during rendering.}

@; =============================================================================
@section[#:tag "mod-build"]{Build System}

@defmodule[camp/build]

The @racketmodname[camp/build] module provides the two-pass build system for Camp sites.

@defproc[(collect [site site?]) site-info?]{
Performs the collect pass over a site. Traverses all documents in each collection, building indexes
for cross-reference resolution: a page index mapping slugs to URLs, a term index mapping term names
to URL fragments, and taxonomy indexes for each collection.}

@defproc[(build! [site site?] [info site-info?]) void?]{
Performs the build pass. Renders all pages using the collected site information, resolves
cross-references, applies render functions, and writes output files.}

@; =============================================================================
@section[#:tag "mod-serve"]{Development Server}

@defmodule[camp/serve]

The @racketmodname[camp/serve] module provides a development server for local testing.

@defproc[(start-server [output-folder path-string?]
                       [#:port port exact-nonnegative-integer? 8000]
                       [#:watch? watch? boolean? #t])
         void?]{
Starts a development server serving files from @racket[_output-folder]. By default, listens on port
8000 and watches for file changes, triggering rebuilds automatically. Set @racket[_watch?] to
@racket[#f] to disable file watching.}

@; =============================================================================
@section[#:tag "mod-log"]{Logging}

@defmodule[camp/log]

The @racketmodname[camp/log] module provides structured logging for Camp operations.

@defthing[camp-logger logger?]{
The Camp logger instance, using the topic @racket['camp]. Subscribe to this logger to receive
Camp-related log messages.}

@defform[(log-camp-fatal string-expr arg ...)]{
Logs a fatal-level message to the Camp logger.}

@defform[(log-camp-error string-expr arg ...)]{
Logs an error-level message to the Camp logger.}

@defform[(log-camp-warning string-expr arg ...)]{
Logs a warning-level message to the Camp logger.}

@defform[(log-camp-info string-expr arg ...)]{
Logs an info-level message to the Camp logger.}

@defform[(log-camp-debug string-expr arg ...)]{
Logs a debug-level message to the Camp logger.}
