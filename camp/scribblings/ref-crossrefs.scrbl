#lang scribble/manual

@(require "doc-util.rkt"
          (for-label camp/xref
                     racket/base
                     (except-in scribble/manual defterm)))

@title[#:tag "mod-xref" #:style 'quiet]{Cross-Reference System}

@defmodule[camp/xref]

The @racketmodname[camp/xref] module provides functions for defining terms and creating
cross-references within Punct source documents. The term system works like Scribble's
@racket[deftech]/@racket[tech]: terms are normalized for lookup, allowing references to match
definitions even with differences in pluralization or capitalization.

@section[#:tag "ref-xref-terms"]{Term definitions and references}

@defproc[(defterm [content any/c] ...) list?]{

Defines a term in a Punct document. The @racket[_content] is displayed as-is and also used to derive
the normalized key for cross-references. Produces a @tt{term-definition} x-expression rendered
as a @tt{<dfn>} element with an @tt{id} anchor.

The anchor key/id is derived from a normalization of the @racket[_content]:

@itemlist[#:style 'ordered
  @item{Case-folded to lowercase}
  @item{Trailing @tt{ies} converted to @tt{y} (e.g., ``libraries'' matches ``library'')}
  @item{Trailing @tt{sses} converted to @tt{ss} (e.g., ``classes'' matches ``class'')}
  @item{Trailing @tt{s} removed, except within @tt{ss} (e.g., ``names'' matches ``name'')}
  @item{Whitespace collapsed and replaced with hyphens}]

}

@defproc[(term [content any/c] ...) list?]{

Produces an element for @racket[content] and hyperlinks it to the @racket[defterm]
definition site of the term. The lookup key is derived from @racket[_content] using the same
normalization process used by @racket[defterm].}

@section[#:tag "ref-xref-page-refs"]{Page References}

@defproc[(page-ref [slug string?] [content any/c] ...) list?]{

Produces a hyperlink to another page using its slug.  Slugs are normalized for consistent
cross-reference resolution:

@itemlist[
  @item{Case-folded to lowercase}
  @item{Non-alphanumeric characters replaced with hyphens}
  @item{Multiple hyphens collapsed to single hyphen}
  @item{Leading/trailing hyphens removed}]

This allows natural references like @code[#:lang "punct"]|{•page-ref{My Page}}| to resolve to a page
with slug @racket["my-page"]. 

If no content is provided, it is used as the link text; otherwise the page's title is used as the
link text.

Slugs should be unique across the entire site. If two pages share the same normalized slug---for
example, pages in different collections with the same filename, or a page whose @tt{slug} meta
overrides to match another page's slug---the later page (in collection order) silently shadows the
earlier one in the page index. A warning is logged during the collect phase when this occurs, but
the build is not halted.}