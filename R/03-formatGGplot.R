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

  if (plotTitle[["text"]] != "") plot <- plot + ggtitle(label = plotTitle[["text"]])
  if (axisTitleX[["text"]] != "") plot <- plot + xlab(axisTitleX[["text"]])
  if (axisTitleY[["text"]] != "") plot <- plot + ylab(axisTitleY[["text"]])

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

  if (!axisRangeX[["fromData"]])
    plot <- plot + xlim(axisRangeX[["min"]], axisRangeX[["max"]])

  if (!axisRangeY[["fromData"]])
    plot <- plot + ylim(axisRangeY[["min"]], axisRangeY[["max"]])

  plot
}

#' Point Style Of GGplot
#'
#' Style of points is defined with \code{pointStyle}. Overwrites previous definitions of \code{geom_point}
#'
#' @param plot (ggplot)
#' @param pointStyle (list) named list with style definitions, or output of \code{plotPointsServer}
#'
#' @export
formatPointsOfGGplot <- function(plot, pointStyle) {
  dataPoints <- pointStyle[["dataPoints"]]

  plot +
    geom_point(shape = dataPoints[["symbol"]],
               size = dataPoints[["size"]],
               colour = dataPoints[["color"]],
               fill = dataPoints[["colorBg"]],
               alpha = ifelse(dataPoints[["hide"]], 0, 1))
}
