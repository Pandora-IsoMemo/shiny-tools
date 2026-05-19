# Custom Points Module

This module provides the UI that allows to add, style and remove custom
points, and returns the config list for the custom points.

Backend for custom points module

## Usage

``` r
customPointsUI(
  id,
  title = "Custom Points",
  titleTag = "h4",
  plot_type = c("ggplot", "base", "none")
)

customPointsServer(
  id,
  plot_type = c("ggplot", "base", "none"),
  custom_points = NULL,
  x_choices = reactive(NULL)
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

- plot_type:

  (character) Type of the plot to add points to, one of "ggplot",
  "base".

- custom_points:

  (reactiveVal) optional custom_points from parent module, this enables
  to change custom_points outside of the module

- x_choices:

  (reactive) (named) character vector to provide choices if x is a
  categorical variable
