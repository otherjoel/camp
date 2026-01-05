// Book template for Camp
// This is imported and called as: #show: book.with(title: ..., ...)

// Cross-reference elements (from Punct sources)
#let term(body) = emph(body)
#let term_definition(body, name: none) = strong(body)

#let book(
  // Required
  title: none,
  authors: (),

  // Optional metadata
  subtitle: none,
  publisher: none,
  edition: none,
  year: none,
  dedication: none,

  // Copyright fields
  copyright-notice: none,
  copyright-isbn-print: none,
  copyright-legal: none,

  // Configuration
  paper-size: "iso-b5",
  font: "TeX Gyre Pagella",

  // The book's content
  body,
) = {
  // Creates a pagebreak to the given parity where empty pages
  // can be detected via `is-page-empty`.
  let detectable-pagebreak(to: "odd") = {
    [#metadata(none) <empty-page-start>]
    pagebreak(to: to)
    [#metadata(none) <empty-page-end>]
  }

  // Workaround for https://github.com/typst/typst/issues/2722
  let is-page-empty() = {
    let page-num = here().page()
    query(<empty-page-start>)
      .zip(query(<empty-page-end>))
      .any(((start, end)) => {
        (start.location().page() < page-num
          and page-num < end.location().page())
      })
  }

  // Set the document's metadata.
  set document(title: title, author: authors.join(", "))

  // Set the body font.
  set text(font: font)

  // Configure the page properties.
  set page(
    paper: paper-size,
    margin: (bottom: 1.75cm, top: 2.25cm),
  )

  // The first page (title page).
  page(align(center + horizon, {
    text(size: 22pt, weight: "bold", title)
    if subtitle != none {
      v(0.5em, weak: true)
      text(size: 14pt, subtitle)
    }
    v(2em, weak: true)
    text(1.6em, authors.join(" and "))
  }))

  // Display publisher info at the bottom of the second page.
  {
    let has-copyright = (
      copyright-notice != none or
      copyright-isbn-print != none or
      copyright-legal != none
    )
    if has-copyright {
      set text(0.8em)
      align(center + bottom, {
        if copyright-notice != none { [#copyright-notice] }
        if copyright-legal != none {
          v(0.5em)
          [#copyright-legal]
        }
        if copyright-isbn-print != none {
          v(0.5em)
          [ISBN: #copyright-isbn-print]
        }
        if publisher != none {
          v(0.5em)
          [#publisher]
          if year != none [ (#year)]
        }
      })
    }
  }

  pagebreak()

  // Display the dedication at the top of the third page.
  if dedication != none {
    v(15%)
    align(center, strong(dedication))
  }

  // Books like their empty pages.
  pagebreak(to: "odd")

  // Configure paragraph properties.
  set par(spacing: 0.78em, leading: 0.78em, first-line-indent: 12pt, justify: true)

  // Start with a chapter outline.
  outline(title: [Chapters])
  pagebreak(to: "odd", weak: true)

  // Configure page properties for the main content.
  set page(
    // The header always contains the book title on odd pages and
    // the author on even pages, unless
    // - we are on an empty page
    // - we are on a page that starts a chapter
    header: context {
      // Is this an empty page inserted to keep page parity?
      if is-page-empty() {
        return
      }

      // Are we on a page that starts a chapter?
      let i = here().page()
      if query(heading).any(it => it.location().page() == i) {
        return
      }

      // Find the heading of the section we are currently in.
      let before = query(selector(heading).before(here()))
      if before != () {
        set text(0.95em)
        let header = smallcaps(before.last().body)
        let the-title = smallcaps(title)
        let author = text(style: "italic", authors.first())
        grid(
          columns: (1fr, 10fr, 1fr),
          align: (left, center, right),
          if calc.even(i) [#i],
          if calc.even(i) { author } else { the-title },
          if calc.odd(i) [#i],
        )
      }
    },
  )

  // Configure chapter headings.
  show heading.where(level: 1): it => {
    // Always start on odd pages.
    detectable-pagebreak(to: "odd")

    // Create the heading numbering.
    let number = if it.numbering != none {
      counter(heading).display(it.numbering)
      h(7pt, weak: true)
    }

    v(5%)
    text(2em, weight: 700, block([#number #it.body]))
    v(1.25em)
  }
  show heading: set text(11pt, weight: 400)

  body
}
