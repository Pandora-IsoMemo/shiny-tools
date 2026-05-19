# Legend Style Of GGplot

Style of legend is defined with argument `legend`. Overwrites previous
definitions of `theme(legend)`

## Usage

``` r
formatLegendOfGGplot(plot, legend, scaleFUN = ggplot2::scale_color_manual, ...)
```

## Arguments

- plot:

  (ggplot) A ggplot object.

- legend:

  (list) named list with style definitions, or output of
  `plotLegendServer`

- scaleFUN:

  (function) function to set scale, e.g.
  [`ggplot2::scale_color_manual`](https://ggplot2.tidyverse.org/reference/scale_manual.html)

- ...:

  additional element specifications not part of base ggplot2. In
  general, these should also be defined in the `element tree` argument.
  [Splicing](https://rlang.r-lib.org/reference/splice.html) a list is
  also supported.

## Value

A ggplot object with updated legend.
