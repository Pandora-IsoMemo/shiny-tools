#' Format Titles Of GGplot
#'
#' @param plot (ggplot)
#' @param text (list) named list with title definitions, output of \code{plotTitlesServer}
#'
#' @export
formatTitlesOfGGplot <- function(plot, text) {
  if (!all(
    names(text) %in% c(
      "plotTitle",
      "xAxisTitle",
      "xAxisText",
      "yAxisTitle",
      "yAxisText",
      "legendTitle",
      "legendText"
    )
  ))
    stop("New element found, please add the new case to 'formatTitlesOfGGplot()' first!")

  # AXES ----
  if (any(grepl("Axis", names(text)))) {
    # set titles
    axisTitleX <- text[["xAxisTitle"]]
    axisTitleY <- text[["yAxisTitle"]]


    if (axisTitleX[["text"]] != "")
      plot <- plot + xlab(axisTitleX[["text"]])
    if (axisTitleY[["text"]] != "")
      plot <- plot + ylab(axisTitleY[["text"]])

    # apply text formatting
    plot <- plot +
      theme(
        axis.title.x = getElementText(axisTitleX),
        axis.title.y = getElementText(axisTitleY),
        axis.text.x = getElementText(text[["xAxisText"]]),
        axis.text.y = getElementText(text[["yAxisText"]])
      )
  }

  # PLOT TITLE ----
  if (any(grepl("plot", names(text)))) {
    plotTitle  <- text[["plotTitle"]]
    if (plotTitle[["text"]] != "")
      plot <- plot + ggtitle(label = plotTitle[["text"]])

    plot <- plot + theme(plot.title   = getElementText(plotTitle))
  }

  # LEGEND ----
  if (any(grepl("legend", names(text)))) {
    plot <- plot +
      theme(legend.title = getElementText(text[["legendTitle"]]),
            legend.text  = getElementText(text[["legendText"]]))
  }

  plot
}

#' Get Element Text
#'
#' @param textDef (list) named list with specs for text formatting, see e.g.
#' \code{config()$defaultGGTitle}
getElementText <- function(textDef = list(fontType = "plain",
                                          color = "#000000",
                                          size = 12L,
                                          hide = FALSE)) {
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
