#' Format Titles Of GGplot
#'
#' Format plot and axis titles and texts of axis labels (and legends) of a ggplot.
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
    plot <- plot %>%
      setCustomTitle(labFun = xlab, label = extractTitle(text[["xAxisTitle"]]))
    plot <- plot %>%
      setCustomTitle(labFun = ylab, label = extractTitle(text[["yAxisTitle"]]))

    # apply text formatting (theme)
    plot <- plot +
      theme(
        axis.title.x = getElementText(text[["xAxisTitle"]]),
        axis.title.y = getElementText(text[["yAxisTitle"]]),
        axis.text.x = getElementText(text[["xAxisText"]]),
        axis.text.y = getElementText(text[["yAxisText"]])
      )
  }

  # PLOT TITLE ----
  if (any(grepl("plot", names(text)))) {
    plot <- plot %>%
      setCustomTitle(labFun = ggtitle, label = extractTitle(text[["plotTitle"]]))

    # apply text formatting (theme)
    plot <- plot + theme(plot.title = getElementText(text[["plotTitle"]]))
  }

  # LEGEND ----
  if (any(grepl("legend", names(text)))) {
    plot <- plot %>%
      setCustomTitle(labFun = labs,
                     color = extractTitle(text[["legendTitle"]]),
                     size = extractTitle(text[["legendTitle"]]),
                     fill = extractTitle(text[["legendTitle"]]),
                     shape = extractTitle(text[["legendTitle"]]))
    # apply text formatting (theme)
    plot <- plot +
      theme(legend.title = getElementText(text[["legendTitle"]]),
            legend.text  = getElementText(text[["legendText"]]))
  }

  plot
}

#' Set Custom Title
#'
#' Set label of a ggplot object
#'
#' @param plot (ggplot)
#' @param labFun (function) function to set label, e.g. \code{xlab}
#' @param ... (list) arguments for \code{labFun}
setCustomTitle <- function(plot, labFun, ...) {
  args <- list(...)
  if (length(args) == 0 || is.null(args[[1]]) || args[[1]] == "") {
    # do not overwrite default title for empty inputs
    return(plot)
  }

  # apply custom title
  plot + labFun(...)
}

#' Extract Title
#'
#' Extract title from list of title definitions
#'
#' @param titleList (list) named list with title definitions, output of \code{plotTitlesServer}
#'
#' @return (character) title
extractTitle <- function(titleList) {
  if (!is.null(titleList[["useExpression"]]) && isTRUE(titleList[["useExpression"]])) {
    return(convertToExpression(titleList[["expression"]]))
  } else {
    res <- ""
    if (!is.null(titleList[["text"]])) res <- titleList[["text"]]

    return(res)
  }
}

#' Get Element Text
#'
#' @param textDef (list) named list with specs for text formatting, see e.g.
#' \code{config()$defaultGGTitle}
getElementText <- function(textDef = list(fontFamily = "sans",
                                          hide = FALSE,
                                          size = 12L,
                                          fontType = "plain",
                                          color = "#000000",
                                          angle = 0,
                                          hjust = 0.5,
                                          vjust = 0.5
                                          )) {
  if (textDef[["hide"]]) {
    element_blank()
  } else {
    element_text(family = textDef[["fontFamily"]],
                 size = textDef[["size"]],
                 face = textDef[["fontType"]],
                 color = textDef[["color"]],
                 angle = textDef[["angle"]],
                 hjust = textDef[["hjust"]],
                 vjust = textDef[["vjust"]])
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

#' Legend Style Of GGplot
#'
#' Style of legend is defined with argument \code{legend}. Overwrites previous definitions of \code{theme(legend)}
#'
#' @param plot (ggplot)
#' @param legend (list) named list with style definitions, or output of \code{plotLegendServer}
#' @inheritParams ggplot2::theme
#'
#' @export
formatLegendOfGGplot <- function(plot, legend, ...) {
  plot +
    theme(legend.position = legend$position, ...)
}
