# Specify Scales of a GGplot

Specify Scales of a GGplot

## Usage

``` r
formatScalesOfGGplot(
  plot,
  ranges,
  xLabels = NULL,
  yLabels = NULL,
  ySecAxisTitle = NULL
)
```

## Arguments

- plot:

  (ggplot) A ggplot object.

- ranges:

  (list) named list with range definitions, output of `plotRangesServer`

- xLabels:

  (list) named list with x axis labels, e.g.
  `list(breaks = c(1, 2, 3), labels = c("A", "B", "C"))`

- yLabels:

  (list) named list with y axis labels, e.g.
  `list(breaks = c(1, 2, 3), labels = c("A", "B", "C"))`

- ySecAxisTitle:

  (character) title of secondary y axis

## Value

A ggplot object with scales formatted.
