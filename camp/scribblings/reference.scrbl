#lang scribble/manual

@(require (for-label camp
                     (except-in camp/page #%module-begin)
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

@subsection{Site Information Parameter}

@defparam[current-site-info info (or/c site-info? #f)]{
A parameter containing the current @racket[site-info] during the build phase. Set by @racket[build!]
before rendering pages. Used internally by @racket[get-collection], @racket[get-taxonomy-terms],
and @racket[get-taxonomy-pages]. Returns @racket[#f] outside of a build context.}

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
@section[#:tag "mod-page"]{Structural Page Language}

@defmodulelang[camp/page]

The @tt{#lang camp/page} language provides an alternative to @tt{#lang punct} for pages that are
primarily structural or organizational---such as tag listings, index pages, and archives---where
CommonMark processing is not needed and full Racket control over the output is desired.

Unlike Punct documents, @tt{#lang camp/page} documents do not pass through a CommonMark parser.
Instead, body expressions are wrapped in a thunk and evaluated at render time when site information
is available. This allows direct use of @racket[get-collection], @racket[get-taxonomy-terms],
@racket[get-taxonomy-pages], and other retrieval functions within the page content.

@subsection{Document Structure}

A @tt{#lang camp/page} document consists of three sections:

@itemlist[#:style 'ordered
  @item{@bold{Module-level forms} (optional): @racket[require], @racket[provide], and @racket[define]
        forms that appear before any metadata. These are lifted to module level and evaluated at load
        time.}
  @item{@bold{Metadata}: Keyword-value pairs like @tt{#:title "Page Title"} that define page
        properties.}
  @item{@bold{Body}: All remaining forms, including any @racket[define] forms after metadata. These
        are wrapped in a thunk and evaluated at render time when @racket[current-site-info] is
        available.}]

@subsection{Example}

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

@subsection{Metadata Keywords}

Metadata is specified using keyword-value pairs. The value following each keyword is read as a Racket
datum.

@tabular[#:sep @hspace[2]
         (list (list @bold{Keyword} @bold{Description})
               (list @tt{#:title} "Page title (used in templates and page index)")
               (list @tt{#:slug} "URL slug (defaults to filename if not specified)")
               (list @tt{#:date} "Publication date (for sorting and feed inclusion)")
               (list @tt{#:draft?} "If @racket[#t], excludes from feeds and navigation")
               (list @tt{#:output-path} "Override the collection's output path pattern"))]

Any other keywords are stored in the document metadata and accessible via @racket[meta-ref].

@subsection{Available Bindings}

The @tt{#lang camp/page} language provides all bindings from @racketmodname[racket/base], plus:

@itemlist[
  @item{All exports from @racketmodname[camp]: @racket[get-collection], @racket[get-taxonomy-terms],
        @racket[get-taxonomy-pages], @racket[page-link-url], @racket[page-link-title],
        @racket[page-link-metas], @racket[prev-in], @racket[next-in], etc.}
  @item{All exports from @racketmodname[punct/doc]: the @racket[document] struct and related
        utilities.}]

Additional modules can be @racket[require]d as needed.

@subsection{Render Function Integration}

Documents written in @tt{#lang camp/page} produce a Punct-compatible @racket[doc] binding with the
body thunk stored in metadata. During the build phase, Camp automatically detects this and calls the
thunk to produce the body content.

@defproc[(camp-page-doc? [doc any/c]) boolean?]{
Returns @racket[#t] if @racket[_doc] is a document produced by @tt{#lang camp/page}, @racket[#f]
otherwise. Use this predicate to handle @tt{#lang camp/page} documents differently from Punct
documents in render functions.}

Render functions can use @racket[camp-page-doc?] to handle @tt{#lang camp/page} documents differently
from Punct documents:

@codeblock|{
(require camp/page)  ; for camp-page-doc?

(define (render-page doc context)
  (define body (hash-ref context 'body))

  (if (camp-page-doc? doc)
      ;; camp/page: body already includes all markup
      (layout (meta-ref doc 'title) `((article ,@body)))
      ;; punct: add title heading
      (layout (meta-ref doc 'title)
              `((article (h1 ,(meta-ref doc 'title)) ,@body)))))
}|

@subsection{Comparison with Punct}

@tabular[#:sep @hspace[2]
         (list (list @bold{Feature} @bold{#lang punct} @bold{#lang camp/page})
               (list "Content format" "Markdown with Racket escapes" "Pure Racket x-expressions")
               (list "CommonMark processing" "Yes" "No")
               (list "Site retrieval functions" "In render function only" "Directly in page body")
               (list "Best for" "Prose-heavy content" "Structural/organizational pages"))]

@; =============================================================================
@section[#:tag "mod-xref"]{Cross-Reference System}

@defmodule[camp/xref]

The @racketmodname[camp/xref] module provides functions for defining terms and creating
cross-references within Punct source documents. The term system works like Scribble's
@tt{deftech}/@tt{tech}: terms are normalized for lookup, allowing references to match definitions
even with differences in pluralization or capitalization.

@subsection{Term Normalization}

Term names are normalized for consistent cross-reference resolution:
@itemlist[
  @item{Case-folded to lowercase}
  @item{Trailing @tt{ies} converted to @tt{y} (e.g., ``libraries'' matches ``library'')}
  @item{Trailing @tt{sses} converted to @tt{ss} (e.g., ``classes'' matches ``class'')}
  @item{Trailing @tt{s} removed, except after @tt{ss} (e.g., ``APIs'' matches ``api'')}
  @item{Whitespace collapsed and replaced with hyphens}]

This allows natural prose like ``the @tt{@"•"term@"{APIs@"}"} we discussed'' to resolve to a
definition of ``API''.

@subsection{Defining Terms}

@defproc[(defterm [content any/c] ...) list?]{
Defines a term in a Punct document. The @racket[_content] is displayed as-is and also used to derive
the normalized key for cross-references. Produces a @racket[term-definition] x-expression rendered
as a @tt{<dfn>} element with an @tt{id} anchor.

In Punct source:
@codeblock|{
•define-term{pianoforte}—the full name of the piano, from Italian
meaning "soft-loud."
}|

The term ``pianoforte'' appears in the prose, and an anchor @tt{#term-pianoforte} is created for
cross-references. The author writes the definition in surrounding prose however they prefer.}

@defproc[(define-term [content any/c] ...) list?]{
Alias for @racket[defterm].}

@subsection{Referencing Terms}

@defproc[(term [content any/c] ...) list?]{
References a previously defined term. The @racket[_content] is displayed as-is; its normalized form
is used to look up the term definition. Produces a @racket[term] x-expression resolved to a
hyperlink during rendering.

In Punct source:
@codeblock|{
A student of the •term{pianoforte} must cultivate patience.
}|

The word ``pianoforte'' appears as a link to the page and anchor where it was defined.

Because of normalization, @tt{@"•"term@"{APIs@"}"} will successfully link to a definition created
with @tt{@"•"define-term@"{API@"}"}---the plural ``APIs'' normalizes to ``api'', matching the
singular definition.}

@subsection{Page References}

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
                       [#:watch? watch? boolean? #t]
                       [#:log-format log-format (or/c 'modern 'apache) 'modern])
         (-> void?)]{
Starts a development server serving files from @racket[_output-folder]. Returns a shutdown procedure
that stops the server when called.

By default, listens on port 8000 and watches for file changes, triggering rebuilds automatically.
Set @racket[_watch?] to @racket[#f] to disable file watching.

The @racket[_log-format] parameter controls request logging: @racket['modern] produces a clean
@tt{HH:MM:SS METHOD PATH STATUS} format suitable for colorized terminal output, while
@racket['apache] produces traditional Apache combined log format.}

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
