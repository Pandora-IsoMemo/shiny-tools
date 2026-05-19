# Server function for plot export

Backend for plot export module

## Usage

``` r
plotExportServer(
  id,
  plotFun,
  plotType = c("none", "ggplot", "ggplot_only_titles"),
  filename = sprintf("%s_plot", gsub("-", "", Sys.Date())),
  plotly = FALSE,
  plotWidth = reactive(1280),
  plotHeight = reactive(800),
  initText = NULL,
  initRanges = NULL
)
```

## Arguments

- id:

  namespace id

- plotFun:

  (reactive) a reactive function returning a plot for export

- plotType:

  (character) one of "none", "ggplot", "ggplot_only_titles". Adds the
  option to format titles and ranges of a plot within the export UI
  (currently only for ggplots). For `plotType == "ggplot_only_titles"`
  only titles can be adjusted. This prevents that custom formatting of
  axis ranges might be overwritten by
  [`formatScalesOfGGplot()`](https://pandora-isomemo.github.io/shiny-tools/reference/formatScalesOfGGplot.md).

- filename:

  (character) name of file without file extension

- plotly:

  (logical) set TRUE if plotFun returns a plotly output

- plotWidth:

  (reactive) default plot width

- plotHeight:

  (reactive) default plot height

- initText:

  (list) optional, named list with title definitions, or output of
  `plotTitlesServer`

- initRanges:

  (list) optional, named list with range definitions, or output of
  `plotRangesServer`
