# Plot Titles UI

Backend for plot titles module

## Usage

``` r
plotTitlesUI(
  id,
  title = "Plot Texts",
  titleTag = "h4",
  type = c("ggplot", "base", "none"),
  initText = NULL
)

plotTitlesServer(
  id,
  type = c("none", "ggplot", "base"),
  availableElements = c("title", "axis"),
  showParseButton = TRUE,
  initText = NULL
)
```

## Arguments

- id:

  namespace id

- title:

  (character) module title

- titleTag:

  (character) HTML tag to put around the title, e.g. "h4" for `h4` from
  `htmltools`

- type:

  (character) Type of the plot to add titles to, one of "none",
  "ggplot", "base".

- initText:

  (list) optional, named list with title definitions, or output of
  `plotTitlesServer`

- availableElements:

  (character) set of available labels for specifying the format of text.
  May contain elements from `c("title", "axis", "yaxis2", "legend")`.

- showParseButton:

  (logical) Show parse button for parsing mathematical expressions.

## Value

tagList
