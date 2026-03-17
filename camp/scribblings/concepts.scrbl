#lang scribble/manual

@(require (for-label camp
                     camp/xref
                     racket/base))

@title[#:tag "basic-concepts"]{Basic Camp Concepts}

@;===============================================

A Camp @tech{site} is a single website, organized as a locally-installed Racket package.

Everything in your Camp site is Racket code: your posts, your configuration, and any custom
functionality you write for it. It only makes sense to instruct Racket to treat all of this related
code as a package. This makes it easier to access your code’s bindings using normal module paths:
for instance, you can start your page sources with @racketmodfont{#lang punct mysite} to bring all
of the bindings from your package’s @filepath{main.rkt} into scope.

Your site provides a configuration module which specifies where your source files live and how they
get transformed into pages for your site.


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
sort-key = "date"
order = "descending"
}|}


The site config groups pages into @tech{collections}. Each collection specifies a name, a folder
where its source files live, a sort order, an output path pattern that specifies the URL format for
its pages, and a function for rendering those files to output HTML files.

Collections use @tech{source path patterns} and @tech{output path patterns} to map source files to
output locations.  In the example above, a source file named @filepath{hello-world.md.rkt} in the
project’s @filepath{posts/} subfolder with a @tt{date} meta of @racket{2026-01-31} will be rendered
to @filepath{posts/2026/01/hello-world/index.html}. 

@section{Source files}

@margin-note{See the @hyperlink["https://joeldueck.com/what-about/punct/Quick_start.html"]{Punct
Quick Start} for a primer on how Punct works.}

Most source files will be written in @hyperlink["https://joeldueck.com/what-about/punct"]{Punct}, a
Racket dialect that transforms Markdown into a format-independent syntax tree. This can simply be
plain Markdown, but your sources can also use inline Racket code to add custom elements.

All source files have a @deftech{slug}, which is a string of characters that uniquely identifies the
file. A slug can be specified manually by giving a @tt{slug} value in the source file’s @tt{metas},
otherwise it is derived from the source’s filename without any extensions.  For example, a source
file named @filepath{first-post.md.rkt} would have a default slug of @tt{first-post}.

For organizational pages that exist mainly as an index of other content (such as a blog archive,
tag index, etc.) you can use @hash-lang[] @racketmodname[camp/page], which lets you specify an HTML
x-expression directly and allows you do do fancy stuff like generate multiple files from a single
source file (such as when creating paginated blog listings).

@section{The build process}

When you run @tt{raco camp build}, Camp processes your site in two passes: a @emph{collect} pass
that reads every source file and builds indexes of your content, followed by a @emph{build} pass
that renders each page to HTML and writes it to the output folder. This two-pass design is
central to how Camp works. By the time any page is rendered, Camp already knows the URL, title, and
metadata for every other page on the site. Cross-references, navigation links, and taxonomy
groupings all resolve correctly because the full picture is assembled before rendering begins.

You don't need to memorize these details to use Camp, but understanding the overall flow helps when
debugging build errors or writing advanced render functions.

@subsection{The collect pass}

The build starts by loading your @filepath{site.rkt} module. Camp uses Racket's
@racket[dynamic-require] to evaluate it at runtime and extract the TOML configuration, producing a
@tech{site} value containing your title, URL, @tech{collections}, @tech{feed} configurations, and
output settings.

Camp then iterates over each collection. For a given collection, it looks in the source directory
specified by the collection's @tech{source path pattern} and finds every file whose extension
matches the site's configured source format (by default, @filepath{.md.rkt}). Each source file is
itself a Racket module. Camp loads it with @racket[dynamic-require] and extracts its @tt{doc}
binding---a Punct @racket[document] containing both metadata and content.

For each source, Camp determines the @tech{page}'s @tech{slug}: either a @tt{slug} value declared
in the document's metadata, or the filename stripped of its extensions. It then computes the output
file path by applying the collection's @tech{output path pattern}, substituting @tt{*} with the
slug and filling any bracketed date codes (like @tt{[yyyy]} or @tt{[MM]}) from the page's @tt{date}
metadata.

Once all sources in a collection are loaded, Camp sorts them by the collection's @tt{sort-key}
(usually @tt{date}) in the configured order. This sort order determines the sequence used for
prev/next navigation within the collection.

After processing every collection, Camp builds three indexes from the collected pages:

@itemlist[

@item{A @bold{page index} mapping each page's normalized @tech{slug} to a @racket[page-link]
containing its URL, title, and metadata. This powers @racket[page-ref] cross-references.}

@item{A @bold{term index} mapping normalized term names to URL fragments. Any source that uses
@racket[defterm] from @racketmodname[camp/xref] registers a term definition during collection.
This powers @racket[term] cross-references.}

@item{A @bold{taxonomy index} that groups pages by their taxonomy metadata. If a collection
declares taxonomies like @tt{tags} or @tt{series}, Camp reads those values from each page and
organizes them into a nested mapping of collection name to taxonomy key to term to page list.}

]

These indexes are the reason for the two-pass architecture. A page might reference a term defined in
a page that hasn't been loaded yet, or link to a page whose output URL depends on another
collection's sort order. By deferring all rendering until the indexes are complete, Camp guarantees
that every cross-reference can be resolved.

@subsection{The build pass}

The build pass begins by copying static assets---stylesheets, images, scripts---from the site's
@tt{static} folder into the output directory, preserving file timestamps so that unchanged assets
don't trigger unnecessary cache invalidation.

Next, Camp pre-computes a @tech{render context} for each page. The context is a lightweight table
containing the page's @tech{slug}, its canonical URL, the name of its collection, and its taxonomy
values. Your render function receives this context alongside the document itself.

For each page, Camp resolves the render function specified by the collection's @tt{render-with}
field (or the site-level @tt{default-render}). This is a Racket function that Camp loads with
@racket[dynamic-require], so it can live in any module your site provides. Camp calls it with two
arguments: the Punct document and the render context. The function returns an x-expression---a
Racket representation of an HTML tree.

Inside the render function, you typically call @racket[camp-doc->html-xexpr] to convert the
document body to HTML elements. This is where cross-references are actually resolved: the
rendering layer looks up each @racket[term] and @racket[page-ref] element in the indexes built
during the collect pass and replaces them with the correct hyperlinks. If a reference can't be
resolved, Camp logs a warning and renders an error marker so you can find and fix it.

The x-expression returned by the render function is converted to a formatted HTML5 string and
written to the computed output path. Camp produces human-readable HTML with proper indentation and
line wrapping, not a compressed single-line blob.

If your site defines any @tech{feed} configurations, Camp generates them after all pages have been
rendered. Each feed re-renders the relevant pages through the feed's own render function to produce
the HTML content of feed entries. Camp uses the Splitflap library to produce spec-compliant Atom or
RSS XML, which is written alongside your HTML output.

Finally, Camp scans the output directory for HTML files that were not generated during this build.
These orphans---left behind when you rename or delete a source file---are removed automatically,
along with any directories left empty by the removal.

The entire process is typically fast. Camp reports timing for each phase so you can see where time
is spent. For iterative work during development, @tt{raco camp serve} watches for file changes and
automatically re-runs the collect and build passes when a source file or template changes.
