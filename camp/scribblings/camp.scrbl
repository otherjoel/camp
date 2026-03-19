#lang scribble/manual

@(require (for-label camp
                     racket/base))

@(require "doc-util.rkt"
          racket/runtime-path
          scribble/core
          scribble/html-properties)

@(define-runtime-path logo "camp-med.png")

@title[#:style 'toc]{Camp: Static Site Generation for Racket}
@author{Joel Dueck}

@image[logo #:scale 0.5 #:style (style #f (list (attributes '((style . "float: right;")))))]

Camp is a static site generator built on Racket. I made it for myself, but if you enjoy the craft
and activity of web and print publishing, you might like it too. It gives you tools and techniques
for building a site or blog that is personal, programmable and permanent.

Camp builds on @hyperlink["https://joeldueck.com/what-about/punct/"]{Punct}, a Racket DSL that lets
you extend Markdown with Racket code, and output to HTML or Typst.

@itemlist[

@item{Camp provides facilities for navigation between posts, cross references and taxonomies (such as
tags or series).}

@item{You can use Camp via the CLI, or via @secref["gui-app"].}

@item{Camp helps you convert collections of posts into print-ready book PDFs via 
  @hyperlink["https://typst.app"]{Typst}.}

@item{Camp produces spec-compliant RSS/Atom feeds. And though many may not notice, it also produces
HTML that is line-wrapped and indented for high readability.}

]

@inline-note{The canonical copy of this documentation is at @url{https://joeldueck.com/what-about/camp}.}

@local-table-of-contents[]

@include-section["quickstart.scrbl"]
@include-section["concepts.scrbl"]
@include-section["tutorial-blog.scrbl"]
@include-section["tutorial-navigation.scrbl"]
@include-section["tutorial-listings.scrbl"]
@include-section["tutorial-book.scrbl"]
@include-section["camp-app.scrbl"]
@include-section["reference.scrbl"]
@include-section["epilogue.scrbl"]