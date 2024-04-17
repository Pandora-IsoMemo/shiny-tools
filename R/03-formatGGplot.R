#' Format Titles Of GGplot
#'
#' @param plot (ggplot)
#' @param text (list) named list with title definitions, output of \code{plotTitlesServer}
#'
#' @export
formatTitlesOfGGplot <- function(plot, text) {
  getElementText <- function(textDef) {
    if (textDef[["hide"]]) {
      element_blank()
    } else {
      element_text(family = "Arial",
                   size = textDef[["size"]],
                   face = textDef[["fontType"]],
                   color = textDef[["color"]],
                   hjust = 0.5)
    }
  }

  if (!all(names(text) %in% c("plotTitle", "xAxisTitle", "xAxisText", "yAxisTitle", "yAxisText",
                              "legendTitle")))
    stop("New element found, please add the new case to 'formatTitlesOfGGplot()' first!")

  if ("legendTitle" %in% names(text))
    message("Note: formatting of element 'legendTitle' not included in 'formatTitlesOfGGplot()'. Please integrate separately.")


  # set titles
  plotTitle  <- text[["plotTitle"]]
  axisTitleX <- text[["xAxisTitle"]]
  axisTitleY <- text[["yAxisTitle"]]

  if (plotTitle[["text"]] != "") plot <- plot + ggtitle(label = plotTitle[["text"]])
  if (axisTitleX[["text"]] != "") plot <- plot + xlab(axisTitleX[["text"]])
  if (axisTitleY[["text"]] != "") plot <- plot + ylab(axisTitleY[["text"]])

  # apply text formatting
  axisTextX <- text[["xAxisText"]]
  axisTextY <- text[["yAxisText"]]

  plot +
    theme(
      plot.title   = getElementText(plotTitle),
      axis.title.x = getElementText(axisTitleX),
      axis.title.y = getElementText(axisTitleY),
      axis.text.x = getElementText(axisTextX),
      axis.text.y = getElementText(axisTextY)
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
#' @inheritParams ggplot2::geom_point
#'
#' @export
formatPointsOfGGplot <- function(plot, data = NULL, pointStyle = NULL, ...) {
  if (is.null(pointStyle)) {
    # if null: take values from config
    pointStyle <- config()$defaultPointStyle
  }

  dataPoints <- pointStyle[["dataPoints"]]

  plot +
    geom_point(data = data,
               shape = dataPoints[["symbol"]],
               size = dataPoints[["size"]],
               colour = dataPoints[["color"]],
               fill = dataPoints[["colorBg"]],
               alpha = ifelse(dataPoints[["hide"]], 0, dataPoints[["alpha"]]),
               ...)
}
