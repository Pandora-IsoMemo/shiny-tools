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
                   color = titleDef[["color"]],
                   hjust = 0.5)
    }
  }

  plotTitle  <- titles[["plot"]]
  axisTitleX <- titles[["xAxis"]]
  axisTitleY <- titles[["yAxis"]]

  if (plotTitle[["text"]] != "") plot <- plot + labs(title = plotTitle[["text"]])
  if (axisTitleX[["text"]] != "") plot <- plot + labs(x = axisTitleX[["text"]])
  if (axisTitleY[["text"]] != "") plot <- plot + labs(y = axisTitleY[["text"]])

  plot +
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

  if (!is.na(axisRangeX[["min"]]) && !is.na(axisRangeX[["max"]]))
    plot <- plot + xlim(axisRangeX[["min"]], axisRangeX[["max"]])

  if (!is.na(axisRangeY[["min"]]) && !is.na(axisRangeY[["max"]]))
    plot <- plot + ylim(axisRangeY[["min"]], axisRangeY[["max"]])

  plot
}
