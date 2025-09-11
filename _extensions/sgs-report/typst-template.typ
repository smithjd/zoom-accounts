// This is an example typst template (based on the default template that ships
// with Quarto). It defines a typst function named 'article' which provides
// various customization options. This function is called from the 
// 'typst-show.typ' file (which maps Pandoc metadata function arguments)
//
// If you are creating or packaging a custom typst template you will likely
// want to replace this file and 'typst-show.typ' entirely. You can find 
// documentation on creating typst templates and some examples here: 
//   - https://typst.app/docs/tutorial/making-a-template/
//   - https://github.com/typst/templates

#let article(
  title: none,
  authors: none,
  date: none,
  file: none,
  abstract: none,
  cols: 1,
  margin: (x: 1.25in, y: 1in),
  paper: "us-letter",
  lang: "en",
  region: "US",
  font: ("Gandhi Sans"),
  fontsize: 14pt,
  sectionnumbering: none,
  toc: false,
  doc,
) = {
  set page(
    paper: paper,
    margin: margin,
    numbering: "1",
    footer: [
      #grid(
        columns: (1fr, 1fr),
        [#if file != none [#file] else [No file parameter]],
        align(right)[#counter(page).display()]
      )
    ],
    background: [
      // Blue border background
      #place(
        left + top,
        dx: 0in,      // Position relative to page edge
        dy: 0in,      // Position relative to page edge
        rect(
          width: 1.1in,
          height: 100%,  // Use 100% to cover full page height
          fill: gradient.linear(
            (rgb("#ADD8E6"), 0%),
            (rgb("#E6F3FF"), 50%),
            (white, 100%),
            angle: -90deg
          )
        )
      )
      // Logo image on top of blue border
      #place(
        left + top,
        dx: 0in,      // Position at left edge
        dy: 0in,      // Position at top edge
        image("_extensions/sgs-report/SI-sunburst.png", width: 1.12in)
      )
    ]
  )
  
  set par(justify: true)
  set text(lang: lang,
           region: region,
           font: font,
           size: fontsize)
  set heading(numbering: sectionnumbering)

  if title != none {
    align(center)[
      #text(weight: "bold", size: 1.5em)[#title]
    ]
  }

  if date != none {
    align(center)[#date]
  }

  if authors != none {
    let count = authors.len()
    let ncols = calc.min(count, 3)
    grid(
      columns: (1fr,) * ncols,
      row-gutter: 1.5em,
      ..authors.map(author =>
          align(center)[
            #author.name \
            #author.affiliation \
            #author.email
          ]
      )
    )
  }

  if abstract != none {
    block(inset: 2em)[
    #text(weight: "semibold")[Abstract] #h(1em) #abstract
    ]
  }

  if toc {
    block(above: 0em, below: 2em)[
    #outline(
      title: auto,
      depth: none
    );
    ]
  }

  if cols == 1 {
    doc
  } else {
    columns(cols, doc)
  }
}