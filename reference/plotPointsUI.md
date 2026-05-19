# Plot Points UI

Plot Points UI

## Usage

``` r
plotPointsUI(
  id,
  title = "Data Points",
  titleTag = "h4",
  type = c("ggplot", "base"),
  initStyle = NULL
)
```

## Arguments

- id:

  module id

- title:

  (character) module title

- titleTag:

  (character) HTML tag to put around the title, e.g. "h4" for `h4` from
  `htmltools`

- type:

  (character) Type of the plot to edit points for, one of "ggplot",
  "base".

- initStyle:

  (list) optional, named list with style definitions, should have the
  same format as the default output of `plotPointsServer`

## Value

tagList
