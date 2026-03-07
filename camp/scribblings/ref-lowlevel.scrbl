#lang scribble/manual

@(require "doc-util.rkt"
          (for-label (except-in racket/base date date?)
                     camp
                     camp/build
                     camp/serve
                     camp/log
                     gregor
                     punct/doc
                     punct/render/typst
                     racket/class))

@title[#:tag "ref-lowlevel"]{Low-Level API}

@local-table-of-contents[]

@; =============================================================================
@section[#:tag "ref-build"]{Collecting and Building}

@defmodule[camp/build]

The @racketmodname[camp/build] module provides the two-pass build system for Camp sites.

@defproc[(collect [site site?]) site-info?]{
Performs the collect pass over a site. Traverses all documents in each collection, building
indexes for cross-reference resolution: a page index mapping slugs to URLs, a term index mapping
term names to URL fragments, and taxonomy indexes for each collection.}

@defproc[(collect/call-with-page [site site?]
                                 [slug string?]
                                 [proc (-> document? context? site? any)])
         any]{
Collects the site, looks up the page matching @racket[_slug], and calls @racket[_proc] with
the page's document, @tech{render context}, and site. The @racket[current-site-info] parameter
is set for the duration of @racket[_proc], so cross-reference functions like
@racket[get-collection] and @racket[get-taxonomy-pages] work normally.

This is useful for rendering or processing a single page with full site context without performing
a complete @racket[build!]. The slug is matched case-insensitively. Raises an error if no page
with the given slug exists.

@codeblock|{
(collect/call-with-page my-site "my-post"
  (λ (doc ctx site)
    (my-render-function doc ctx)))
}|}

@defproc[(build! [site site?] [info site-info?]) void?]{
Performs the build pass. For each page, calls its collection's render function with the document
and context. Render functions call @racket[camp-doc->html-xexpr] to render the body (which
resolves cross-references). Writes the returned x-expression to the output folder as HTML.}

@defstruct[site-info ([pages (listof page?)]
                      [term-index hash?]
                      [page-index hash?]
                      [taxonomy-index hash?])
                     #:transparent]{
Collected information about a site, built during the collect pass. Contains all processed pages,
an index mapping term names to URLs, an index mapping slugs to page-links, and a nested index for
taxonomy lookups.}

@defparam[current-site-info info (or/c site-info? #f)]{
A parameter containing the current @racket[site-info] during the build phase. Set by
@racket[build!] before rendering pages. Used internally by @racket[get-collection],
@racket[get-taxonomy-terms], and @racket[get-taxonomy-pages]. Returns @racket[#f] outside of a
build context.}

@defstruct[page ([source-path path?]
                 [output-path path?]
                 [doc any/c]
                 [slug string?]
                 [collection-name string?])
                #:transparent]{
Represents a page during the build process. Contains the source file path, computed output path,
the Punct document, URL slug, and the name of the collection it belongs to.}

@; =============================================================================
@section[#:tag "ref-serve"]{Development Server}

@defmodule[camp/serve]

The @racketmodname[camp/serve] module provides a development server for local testing.

@defproc[(start-server [output-folder path-string?]
                       [#:port port exact-nonnegative-integer? 8000]
                       [#:log-format log-format (or/c 'modern 'apache) 'modern])
         (-> void?)]{
Starts a static file server serving files from @racket[_output-folder]. Returns a shutdown
procedure that stops the server when called. By default, listens on port 8000.

The @racket[_log-format] parameter controls request logging: @racket['modern] produces a clean
@tt{HH:MM:SS METHOD PATH STATUS} format suitable for colorized terminal output, while
@racket['apache] produces traditional Apache combined log format.

Note: This function only starts a static server. File watching and automatic rebuilds are provided
by the @tt{raco camp serve} command, which uses this function internally.}

@; =============================================================================
@section[#:tag "ref-log"]{Logging}

@defmodule[camp/log]

The @racketmodname[camp/log] module provides structured logging for Camp operations.

@defthing[camp-logger logger?]{
The Camp logger instance, using the topic @racket['camp]. Subscribe to this logger to receive
Camp-related log messages.}

@deftogether[(@defform[(log-camp-fatal string-expr arg ...)]
              @defform[(log-camp-error string-expr arg ...)]
              @defform[(log-camp-warning string-expr arg ...)]
              @defform[(log-camp-info string-expr arg ...)]
              @defform[(log-camp-debug string-expr arg ...)])]{

Log messages of varying severity levels to the Camp logger.

}
