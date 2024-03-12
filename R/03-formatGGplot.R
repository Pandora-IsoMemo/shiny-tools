formatWrapperGGplot <- function(plot, titles, ranges) {
  plot %>%
    formatTitlesOfGGplot(titles = titles) %>%
    formatRangesOfGGplot(ranges = ranges)
}

#' Format Titles Of GGplot
#'
#' @param plot (ggplot)
#' @param titles (list) named list with title definitions, output of \code{plotTitlesServer}
#'
#' @export
formatTitlesOfGGplot <- function(plot, titles) {
  getElementText <- function(titleDef) {
    if (titleDef[["hide"]]) {
      element_blank()
    } else {
      element_text(family = "Arial",
                   size = titleDef[["size"]],
                   face = titleDef[["fontType"]],
                   color = titleDef[["color"]])
    }
  }

  plotTitle  <- titles[["plot"]]
  axisTitleX <- titles[["xAxis"]]
  axisTitleY <- titles[["yAxis"]]

  plot +
    labs(title = plotTitle[["text"]], x = axisTitleX[["text"]], y = axisTitleY[["text"]]) +
    theme(
      plot.title   = getElementText(plotTitle),
      axis.title.x = getElementText(axisTitleX),
      axis.title.y = getElementText(axisTitleY)
    )
}

#' Axes Ranges Of GGplot
#'
#' @param plot (ggplot)
#' @param ranges (list) named list with range definitions, output of \code{plotRangesServer}
#'
#' @export
formatRangesOfGGplot <- function(plot, ranges) {
  axisRangeX <- ranges[["xAxis"]]
  axisRangeY <- ranges[["yAxis"]]

  plot +
    xlim(axisRangeX[["min"]], axisRangeX[["max"]]) +
    ylim(axisRangeY[["min"]], axisRangeY[["max"]])
}
