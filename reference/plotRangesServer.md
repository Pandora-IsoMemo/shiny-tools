# Server function for plot ranges

Backend for plot ranges module

## Usage

``` r
plotRangesServer(
  id,
  type = c("none", "ggplot", "base"),
  initRanges = NULL,
  axes = c(`x axis` = "xAxis", `y axis` = "yAxis")
)
```

## Arguments

- id:

  namespace id

- type:

  (character) type of the plot to add ranges to, one of "ggplot",
  "base".

- initRanges:

  (list) optional, named list with range definitions, or output of
  `plotRangesServer`

- axes:

  (character) named vector of axes to add ranges to, e.g.
  `c("x axis" = "xAxis", "y axis" = "yAxis", "2nd y axis" = "yAxis2")`.
