#lang scribble/manual

@(require (for-label camp
                     racket/base))

@title[#:tag "basic-concepts"]{Basic Camp Concepts}

@;===============================================

@section{Your site is a Racket package}

Everything in your Camp site is Racket code: your posts, your configuration, and any custom
functionality you write for it. It only makes sense to instruct Racket to treat all of this related
code as a package. 

By installing your site locally as a Racket package, you make it easy to refer to your code using
global identifiers: for instance, you can start your page sources with @hash-lang[]
@racketmodname[punct] @racketmodfont{mysite} to bring all of the bindings from your package’s
@filepath{main.rkt} into scope. 

@;===============================================

@section{You write your documents in Punct}

A @deftech{source file} is a file written in @hash-lang[] @racketmodname[punct], and which has a
particular file extension (@filepath{.md.rkt} by default, but configurable). Each source file
corresponds to a single @filepath{.html} output file.

All source files have a @deftech{slug}, which is a string of characters that uniquely identifies the
file in its @tech{collection}. A slug can be specified manually by giving a @tt{slug} value in the
source file’s @tt{metas}, otherwise it is derived from the source’s filename without any extensions.
For example, a source file named @filepath{first-post.md.rkt} would have a default slug of
@tt{first-post}.

@hyperlink["https://joeldueck.com/what-about/punct"]{Punct} is a Racket dialect that transforms
Markdown into a format-independent syntax tree. See the
@hyperlink["https://joeldueck.com/what-about/punct/Quick_start.html"]{Punct Quick Start} for a primer
on how Punct works.

@bold{Punct is better than Markdown because you can embed Racket code.} You can write your sources
in vanilla Markdown if you wish, but escaping to Racket lets you add cross-references, custom
elements, and anything else a general-purpose programming language can do.

@section{You can create auxiliary pages with @tt{camp/page}}

A @deftech{page source file} is a file written in @hash-lang[] @racketmodname[camp/page], and which
has a @filepath{.page.rkt} extension. A page source may correspond to a single @filepath{.html} output
file, or to multiple output files (such as when creating paginated blog listings).

Page source files are for those organizational pages that exist mainly as an interface to other
content. For example, blog archive listings, tag indexes, etc.

@section{Collections organize source files}

A @tech{collection} is a group of @tech{source files} and/or @tech{page source files} in a
particular folder. A collection gives the source files an ordering based on date, title, or other
metadata, and a mapping from source to output paths.

Collections use @tech{source path patterns} and @tech{output path patterns} to map source files to
output locations, and specify which functions will be used to render sources to HTML X-expressions.

For example:

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

Here, a source file named @filepath{hello-world.md.rkt} in the project’s @filepath{posts/} subfolder
with a @tt{date} meta of @racket{2026-01-31} will be rendered to
@filepath{posts/2026/01/hello-world/index.html}. The contents of that file will be determined by the
result of applying @racket[render-post] from the project’s @filepath{render.rkt} module to the source
file’s document and render context.

@itemlist[#:style 'compact

@item{Any folder name consisting of valid CIDR syntax inside a pair of brackets 
@litchar{[]} will be replaced by a string of the corresponding info from the source’s @tt{date} meta.}
                                                                                                     
@item{Any folder name consisting only of @litchar{*} will be replaced by the source’s @tech{slug}.}

@item{If the pattern ends in a slash @litchar{/}, the output file will be named @filepath{index.html}.
Otherwise the output is the name of the pattern’s final element with an added @filepath{.html}
extension.}

]

