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

@youtube-embed-element{https://www.youtube.com/embed/NGhXYoVbLVM}

@local-table-of-contents[]

@section{For whom is Camp?}

Camp is likely to be a good fit for you if:

@itemlist[#:style 'ordered
          
@item{@bold{You want to dual-publish the same writings to the web and to print.} Camp’s processing
pipeline is partly designed to handle the complexity inherent in publishing to multiple output
formats. If you don’t care about this, you will probably find certain parts of Camp a little
complicated at first. Camp works just fine for web-only projects, but it wants you to at least
consider, at the start of your project, whether you might someday want to publish to print as
well as to the web.}

@item{@bold{The thing you want to publish is more like a book than an app, and more like a blog than a
book.} Camp is for publishing open-ended collections of writing. It makes authoring more like
programming, but the resulting website will not be sprinkled with dynamic interactivity fairy
dust.}

@item{@bold{You like Markdown as a starting point, but you also want extensibility without flakiness.}
Maybe, like me, you’ve tried cobbling pandoc together with shell scripts and string processing,
you’ve had to compare the brittle edges of fifteen different Markdown editors and processors to
figure out the magic combination that works for you, and you’re tired. You want extensibility in
the form of an actual programming paradigm.}

@item{@bold{You are handy with Racket (or a sibling language like Scheme or Common Lisp) and
@|X-expression|s.} A Camp site is a Racket programming project. These docs will not teach you
Racket. If you are coming in blind, I highly recommend first learning
@hyperlink["https://docs.racket-lang.org/pollen/"]{Pollen} and following its excellent tutorials.}

@item{@bold{You know HTML and CSS pretty well.} Camp can give you a default theme, but if you need to
customize anything at all, you will be editing CSS by hand.}
                                                                  
]


@include-section["quickstart.scrbl"]
@include-section["concepts.scrbl"]
@include-section["tutorial-blog.scrbl"]
@include-section["tutorial-navigation.scrbl"]
@include-section["tutorial-listings.scrbl"]
@include-section["tutorial-book.scrbl"]
@include-section["camp-app.scrbl"]
@include-section["reference.scrbl"]
@include-section["epilogue.scrbl"]