formatWrapperGGplot <- function(plot, text, ranges) {
  plot %>%
    formatTitlesOfGGplot(text = text) %>%
    formatRangesOfGGplot(ranges = ranges)
}

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

  plotTitle  <- text[["plotTitle"]]
  axisTitleX <- text[["xAxisTitle"]]
  axisTitleY <- text[["yAxisTitle"]]
  axisTextX <- text[["xAxisText"]]
  axisTextY <- text[["yAxisText"]]

  if (plotTitle[["text"]] != "") plot <- plot + ggtitle(label = plotTitle[["text"]])
  if (axisTitleX[["text"]] != "") plot <- plot + xlab(axisTitleX[["text"]])
  if (axisTitleY[["text"]] != "") plot <- plot + ylab(axisTitleY[["text"]])

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
