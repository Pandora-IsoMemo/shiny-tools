# Server function for plot points

Backend for plot points module

## Usage

``` r
plotPointsServer(
  id,
  type = c("ggplot", "base"),
  hideInput = c(),
  initStyle = NULL,
  reloadInit = reactiveVal(FALSE)
)
```

## Arguments

- id:

  namespace id

- type:

  (character) Type of the plot to edit points for, one of "ggplot",
  "base".

- hideInput:

  (character) inputs that should be disabled (hidden) when applying this
  module. Possible inputs are "hide", "symbol", "color", "size",
  "alpha", "colorBg", "lineWidthBg". Please use
  [`shinyjs::useShinyjs()`](https://rdrr.io/pkg/shinyjs/man/useShinyjs.html)
  in your UI function to enable this feature.

- initStyle:

  (list) optional, named list with style definitions, should have the
  same format as the default output of `plotPointsServer`

- reloadInit:

  (reactiveVal) logical, should `initStyle` be reloaded?
