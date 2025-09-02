#' GGPlot Formatting Class
#'
#' This script defines the S3 class `PlotGG` and its methods for formatting ggplot2 plots.
#' This class is a wrapper around the functions for formatting ggplots stored in 03-formatGGplot.R

#' Constructor for PlotGG
#'
#' This function creates a new instance of the PlotGG class.
#' @param plot A ggplot object.
#' @return An object of class `PlotGG`.
#' @export
new_PlotGG <- function(plot) {
  stopifnot(inherits(plot, "gg"))
  structure(list(plot = plot), class = c("PlotGG", "Plot"))
}

# ------- Generics (export these) -------

#' Format plot titles (generic)
#' @param plot_obj A plot-like object.
#' @param text A named list with title/text specs.
#' @param ... Passed on to methods.
#' @export
formatTitles <- function(plot_obj, text, ...) UseMethod("formatTitles")

#' Format plot scales (generic)
#' @param plot_obj A plot-like object.
#' @param ranges A named list with axis ranges/transforms.
#' @param ... Passed on to methods.
#' @export
formatScales <- function(plot_obj, ranges, ...) UseMethod("formatScales")

#' Add custom points (generic)
#' @param plot_obj A plot-like object.
#' @param custom_points A list/data for points.
#' @param ... Passed on to methods.
#' @export
addCustomPoints <- function(plot_obj, custom_points, ...) UseMethod("addCustomPoints")

#' Format legend (generic)
#' @param plot_obj A plot-like object.
#' @param legend A legend configuration list.
#' @param ... Passed on to methods.
#' @export
formatLegend <- function(plot_obj, legend, ...) UseMethod("formatLegend")

#' Coerce to ggplot (generic)
#' @param obj A plot-like object.
#' @export
as.ggplot <- function(obj) UseMethod("as.ggplot")

# ------- S3 methods (export + method tags) -------

#' Format titles for PlotGG
#' @param plot_obj (ggplot)
#' @param text (list) named list with title definitions, output of \code{plotTitlesServer}
#' @param ... Additional arguments (not used).
#' @return A `PlotGG` object (modified).
#' @method formatTitles PlotGG
#' @export
formatTitles.PlotGG <- function(plot_obj, text, ...) {
  plot <- plot_obj$plot

  checkExpectedTitleElements(text)

  plot <- formatTitlesOfGGplot_internal(plot, text)
  # plot <- applyAxisTitles(plot, text)
  # plot <- applyPlotTitle(plot, text)

  plot_obj$plot <- plot
  plot_obj
}

#' Format scales for PlotGG
#'
#' @param plot_obj (ggplot)
#' @param ranges (list) named list with range definitions, output of \code{plotRangesServer}
#' @param xLabels (list) named list with x axis labels, e.g. \code{list(breaks = c(1, 2, 3), labels = c("A", "B", "C"))}
#' @param yLabels (list) named list with y axis labels, e.g. \code{list(breaks = c(1, 2, 3), labels = c("A", "B", "C"))}
#' @param ySecAxisTitle (character) title of secondary y axis
#' @param ... Passed on to [formatScales.PlotGG()].
#' @return A `PlotGG` object (modified).
#' @method formatScales PlotGG
#' @export
formatScales.PlotGG <- function(plot_obj, ranges, xLabels = NULL, yLabels = NULL, ySecAxisTitle = NULL, ...) {
  plot <- plot_obj$plot

  if (is.null(plot) || length(ranges) == 0) return(plot)

  plot <- formatScalesOfGGplot_internal(plot, ranges, xLabels, yLabels, ySecAxisTitle)
  # plot <- applyAxisScales(plot, "x", ranges[["xAxis"]], xLabels)
  # plot <- applyAxisScales(plot, "y", ranges[["yAxis"]], yLabels, ySecAxisTitle)

  plot_obj$plot <- plot
  plot_obj
}

#' Add custom points for PlotGG
#' @param plot_obj (ggplot)
#' @param custom_points A list/data describing points.
#' @param ... Passed on to [addCustomPoints.PlotGG()].
#' @return A `PlotGG` object (modified).
#' @method addCustomPoints PlotGG
#' @export
addCustomPoints.PlotGG <- function(plot_obj, custom_points, ...) {
  plot_obj$plot <- addCustomPointsToGGplot_internal(plot_obj$plot, custom_points)
  #plot_obj$plot <- applyCustomPoints(plot_obj$plot, custom_points)
  plot_obj
}

#' Format legend for PlotGG
#' @param plot_obj (ggplot)
#' @param legend (list) named list with style definitions, or output of \code{plotLegendServer}
#' @param scaleFUN (function) function to set scale, e.g. \code{ggplot2::scale_color_manual}
#' @inheritParams ggplot2::theme
#' @return A `PlotGG` object (modified).
#' @method formatLegend PlotGG
#' @export
formatLegend.PlotGG <- function(plot_obj, legend, scaleFUN = ggplot2::scale_color_manual, ...) {
  #plot_obj$plot <- applyLegend(plot_obj$plot, legend, scaleFUN, ...)
  plot_obj$plot <- formatLegendOfGGplot_internal(plot_obj$plot, legend, scaleFUN, ...)
  plot_obj
}

#' Coerce PlotGG to ggplot
#' @inheritParams as.ggplot
#' @return A `ggplot` object.
#' @method as.ggplot PlotGG
#' @export
as.ggplot.PlotGG <- function(obj) obj$plot


# Helpers for refactoring the  logic ----
# we may add this functions later diectly into functions from formatGGplot.R
checkExpectedTitleElements <- function(text) {
  expected <- c("plotTitle", "xAxisTitle", "xAxisText", "yAxisTitle", "yAxisText",
                "legendTitle", "legendText", "yAxisTitle2", "yAxisText2")
  extra <- setdiff(names(text), expected)
  if (length(extra) > 0) {
    stop("Unexpected elements in title config: ", paste(extra, collapse = ", "))
  }
}

applyAxisTitles <- function(plot, text) {
  if (!any(grepl("Axis", names(text)))) return(plot)

  plot <- plot %>%
    setCustomTitle(xlab, extractTitle(text$xAxisTitle)) %>%
    setCustomTitle(ylab, extractTitle(text$yAxisTitle)) +
    theme(
      axis.title.x = getElementText(text$xAxisTitle),
      axis.title.y = getElementText(text$yAxisTitle),
      axis.title.y.right = getElementText(text$yAxisTitle2),
      axis.text.x = getElementText(text$xAxisText),
      axis.text.y = getElementText(text$yAxisText),
      axis.text.y.right = getElementText(text$yAxisText2)
    )
  plot
}

applyPlotTitle <- function(plot, text) {
  if (!"plotTitle" %in% names(text)) return(plot)

  plot <- plot %>%
    setCustomTitle(ggtitle, extractTitle(text$plotTitle)) +
    theme(plot.title = getElementText(text$plotTitle))
  plot
}

applyAxisScales <- function(plot, axis_type, range, labels, secondary_title = NULL) {
  if (axis_type == "x") {
    if (isContinuousAxis(plot, axis_type)) {
      plot <- plot + scale_x_continuous(
        trans = getTransform(range),
        limits = getUserLimits(range),
        breaks = getBreaks(labels),
        labels = getLabels(labels)
      )
    } else if (isDiscreteAxis(plot, axis_type)) {
      plot <- plot + scale_x_discrete(
        breaks = getBreaks(labels),
        labels = getLabels(labels)
      )
    }
  }
  if (axis_type == "y") {
    limitsY <- getUserLimits(range)
    if (is.null(limitsY)) limitsY <- getGGPlotLimits(plot, axis = "y")
    rescale <- calculateRescalingFactors(limitsY, getUserLimits(range))

    if (isContinuousAxis(plot, axis_type)) {
      plot <- plot + scale_y_continuous(
        trans = getTransform(range),
        limits = limitsY,
        breaks = getBreaks(labels),
        labels = getLabels(labels),
        sec.axis = getSecAxis(rescale, secondary_title)
      )
    } else if (isDiscreteAxis(plot, axis_type)) {
      plot <- plot + scale_y_discrete(
        breaks = getBreaks(labels),
        labels = getLabels(labels),
        sec.axis = getSecAxis(rescale, secondary_title)
      )
    }
  }
  plot
}

applyLegend <- function(plot, legend, scaleFUN = ggplot2::scale_color_manual, ...) {
  legend_title <- extractTitle(legend$layout$title[[1]], default = names(legend$layout$title))
  legend_labels <- sapply(names(legend$layout$labels), function(name) {
    extractTitle(legend$layout$labels[[name]], default = name)
  })
  color_mapping <- extractColourMapping(plot)

  plot <- plot + scaleFUN(labels = legend_labels, values = color_mapping)
  plot <- setLegendThemeOfGGplot(plot, legend, ...)
  plot <- setCustomTitle(plot, labs,
                         color = legend_title,
                         size = legend_title,
                         fill = legend_title,
                         shape = legend_title)
  plot
}

applyCustomPoints <- function(plot, custom_points) {
  if (length(custom_points) == 0 || !is.list(custom_points)) return(plot)
  if ((length(custom_points) > 1 &&
       !all(sapply(custom_points, length) == length(custom_points[[1]])))) return(plot)

  point_df <- custom_points %>% lapply(FUN = as.data.frame) %>% bind_rows()

  plot <- plot %>%
    formatPointErrorsOfGGplot(dat = point_df, style = point_df %>% extractStyleList("error_")) %>%
    formatPointsOfGGplot(data = point_df, pointStyle = point_df %>% extractStyleList("point_"))

  # label style: grab columns starting with "label_"
  label_cols  <- grep("^label_", names(point_df), value = TRUE)
  label_style <- as.list(point_df[label_cols])
  names(label_style) <- sub("^label_", "", names(label_style))

  plot <- plot %>% formatPointLabelsOfGGPlot(data = point_df, labelStyle = label_style)
  plot
}
