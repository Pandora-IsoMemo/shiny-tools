# Plot Legend UI

Function to create the UI for the legend

Function to create the server for the legend

## Usage

``` r
plotLegendUI(
  id,
  title = NULL,
  titleTag = "h4",
  type = c("ggplot", "base", "none"),
  width = "100%"
)

plotLegendServer(
  id,
  legend_title = reactive(NULL),
  legend_labels = reactive(NULL),
  plot_type = c("ggplot", "base", "none")
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

  character. Type of plot. Default is "ggplot".

- width:

  character. Width of the input.

- legend_title:

  reactive. Default title of the legend.

- legend_labels:

  reactive. Default labels of the legend.

- plot_type:

  character. Type of plot. Default is "ggplot".
