# TITLES ----

#' Format Titles Of GGplot
#'
#' Format plot and axis titles and texts of axis labels (and legends) of a ggplot.
#'
#' @param plot (ggplot)
#' @param text (list) named list with title definitions, output of \code{plotTitlesServer}
#'
#' @export
formatTitlesOfGGplot <- function(plot, text) {
  # check if all elements are present
  if (!all(
    names(text) %in% c(
      "plotTitle",
      "xAxisTitle",
      "xAxisText",
      "yAxisTitle",
      "yAxisText",
      "legendTitle",
      "legendText",
      "yAxisTitle2",
      "yAxisText2"
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
        axis.title.y.right = getElementText(text[["yAxisTitle2"]]),
        axis.text.x = getElementText(text[["xAxisText"]]),
        axis.text.y = getElementText(text[["yAxisText"]]),
        axis.text.y.right = getElementText(text[["yAxisText2"]])
      )
  }

  # PLOT TITLE ----
  if (any(grepl("plot", names(text)))) {
    plot <- plot %>%
      setCustomTitle(labFun = ggtitle, label = extractTitle(text[["plotTitle"]]))

    # apply text formatting (theme)
    plot <- plot + theme(plot.title = getElementText(text[["plotTitle"]]))
  }

  plot
}

# Set Custom Title (no docu for 'man' because it is a helper function)
#
# Set label of a ggplot object
#
# @param plot (ggplot)
# @param labFun (function) function to set label, e.g. \code{xlab}
# @param ... (list) arguments for \code{labFun}
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
#' Extract title from list of title definitions. If set to use an expression, convert to expression.
#'
#' @param titleList (list) named list with title definitions, output of \code{plotTitlesServer}
#' @param default (character) default title if no title is set
#'
#' @return (character) title
#'
#' @export
extractTitle <- function(titleList, default = "") {
  if (!is.null(titleList[["useExpression"]]) && isTRUE(titleList[["useExpression"]])) {
    return(convertToExpression(titleList[["expression"]]))
  } else {
    if (is.null(titleList[["text"]]) || titleList[["text"]] == "") {
      return(default)
    } else {
      return(titleList[["text"]])
    }
  }
}

# Get Element Text (no docu for 'man' because it is a helper function)
#
# @param textDef (list) named list with specs for text formatting, see e.g.
# \code{config()$defaultGGTitle}
getElementText <- function(textDef = list(fontFamily = "sans",
                                          hide = FALSE,
                                          size = 12L,
                                          fontType = "plain",
                                          color = "#000000",
                                          angle = 0,
                                          hjust = 0.5,
                                          vjust = 0.5
                                          )) {
  if (is.null(textDef) || textDef[["hide"]]) {
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


# SCALES / RANGES ----

#' Specify Scales of a GGplot
#'
#' @param plot (ggplot)
#' @param ranges (list) named list with range definitions, output of \code{plotRangesServer}
#' @param xLabels (list) named list with x axis labels, e.g. \code{list(breaks = c(1, 2, 3), labels = c("A", "B", "C"))}
#' @param yLabels (list) named list with y axis labels, e.g. \code{list(breaks = c(1, 2, 3), labels = c("A", "B", "C"))}
#' @param ySecAxisTitle (character) title of secondary y axis
#'
#' @export
formatScalesOfGGplot <- function(plot, ranges, xLabels = NULL, yLabels = NULL, ySecAxisTitle = NULL) {
  if (isContinuousAxis(plot, axis = "x")) {
    plot <- plot + scale_x_continuous(trans = getTransform(ranges[["xAxis"]]),
                                      limits = getUserLimits(ranges[["xAxis"]]),
                                      breaks = getBreaks(xLabels),
                                      labels = getLabels(xLabels))
  }

  if (isDiscreteAxis(plot, axis = "x")) {
    plot <- plot + scale_x_discrete(breaks = getBreaks(xLabels),
                                    labels = getLabels(xLabels))
  }

  # ensure valid y limits
  limitsY <- getUserLimits(ranges[["yAxis"]])
  if (is.null(limitsY)) {
    limitsY <- getGGPlotLimits(plot, axis = "y")
  }

  rescalingFactors <- calculateRescalingFactors(oldLimits = limitsY,
                                                newLimits = getUserLimits(ranges[["yAxis2"]]))

  if (isContinuousAxis(plot, axis = "y")) {
    plot <- plot + scale_y_continuous(trans = getTransform(ranges[["yAxis"]]),
                                      limits = limitsY,
                                      breaks = getBreaks(yLabels),
                                      labels = getLabels(yLabels),
                                      sec.axis = getSecAxis(rescalingFactors, ySecAxisTitle))
  }

  if (isDiscreteAxis(plot, axis = "y")) {
    plot <- plot + scale_y_discrete(breaks = getBreaks(yLabels),
                                    labels = getLabels(yLabels),
                                    sec.axis = getSecAxis(rescalingFactors, ySecAxisTitle))
  }

  plot
}

isContinuousAxis <- function(plot, axis = c("x", "y")) {
  axis <- match.arg(axis)
  axis_data <- eval_tidy(plot$mapping[[axis]], plot$data)
  is.numeric(axis_data)
}

isDiscreteAxis <- function(plot, axis = c("x", "y")) {
  axis <- match.arg(axis)
  axis_data <- eval_tidy(plot$mapping[[axis]], plot$data)
  is.factor(axis_data) || is.character(axis_data)
}

#' Axes Ranges Of GGplot (deprecated)
#'
#' This function is deprecated. Please use `formatScalesOfGGplot` instead.
#'
#' @param ... arguments of \code{formatScalesOfGGplot}
#' @seealso \link[shinyTools]{formatScalesOfGGplot}
#'
#' @export
formatRangesOfGGplot <- function(...) {
  deprecate_warn("24.10.0", "formatRangesOfGGplot()", "formatScalesOfGGplot()")
  formatScalesOfGGplot(...)
}

#' Calculate Rescaling Factors
#'
#' @param oldLimits (numeric) old limits
#' @param newLimits (numeric) new limits
#'
#' @return (list) with scale and center
#'
#' @export
calculateRescalingFactors <- function(oldLimits, newLimits = NULL) {
  if (length(oldLimits) < 2 || length(newLimits) < 2) return(list(scale = 1, center = 0))

  b <- seq(min(newLimits), max(newLimits), length.out = 100)
  a <- seq(min(oldLimits), max(oldLimits), length.out = 100)
  res <- lm(b ~ a)

  list(scale = res$coefficients[2],
       center = res$coefficients[1])
}

getGGPlotLimits <- function(plot, axis = c("x", "y")) {
  axis <- match.arg(axis)

  if (!is.null(plot$coordinates$limits[[axis]])) return(range(plot$coordinates$limits[[axis]]))

  plotData <- ggplot_build(plot)
  if (!is.null(plotData[["data"]][[1]][[axis]])) return(range(plotData[["data"]][[1]][[axis]]))

  # plot limits not found: return NULL
  return(NULL)
}

getUserLimits <- function(axisFormat) {
  if (is.null(axisFormat[["fromData"]]) || axisFormat[["fromData"]]) {
    return(NULL)
  } else {
    c(axisFormat[["min"]], axisFormat[["max"]])
  }
}

getTransform <- function(axisFormat) {
  if (is.null(axisFormat[["transform"]])) {
    return("identity")
  } else {
    switch(axisFormat[["transform"]],
           "pseudo-log" = pseudo_log_trans(),
           "sqrt" = sqrt_trans(),
           axisFormat[["transform"]])
  }
}

getBreaks <- function(labels) {
  if (is.null(labels) || is.null(labels[["breaks"]])) {
    ggplot2::waiver()
  } else {
    labels[["breaks"]]
  }
}

getLabels <- function(labels) {
  if (is.null(labels) || is.null(labels[["labels"]])) {
    ggplot2::waiver()
  } else {
    labels[["labels"]]
  }
}

getSecAxis <- function(rescaleFactors, title) {
  if (is.null(rescaleFactors) || is.null(title) || is.null(rescaleFactors[["center"]]) || is.null(rescaleFactors[["scale"]])) {
    ggplot2::waiver()
  } else {
    center <- rescaleFactors[["center"]]
    scale <- rescaleFactors[["scale"]]
    ggplot2::sec_axis(~(.* scale) + center, name = title)
  }
}

# POINTS ----

#' Add Custom Points To GGplot
#'
#' Add custom points to a ggplot.
#'
#' @param plot (ggplot)
#' @param custom_points (list) named list with point definitions, output of \code{customPointsServer}
#'
#' @export
addCustomPointsToGGplot <- function(plot, custom_points) {
  if (length(custom_points) == 0) return(plot)

  point_df <- custom_points %>% lapply(FUN = as.data.frame) %>% bind_rows()

  ## add errors
  plot <- plot %>%
    formatPointErrorsOfGGplot(dat = point_df,
                              style = point_df %>% extractStyleList(prefix = "error_")) %>%
    suppressWarnings()

  # add points
  plot <- plot %>%
    formatPointsOfGGplot(data = point_df,
                         pointStyle = point_df %>% extractStyleList(prefix = "point_"),
                         aes(x = .data$x, y = .data$y))

  # add labels
  label_style <- point_df %>%
    dplyr::select(dplyr::starts_with("label_")) %>%
    as.list()
  names(label_style) <- gsub("label_", "", names(label_style))

  plot <- plot %>%
    formatPointLabelsOfGGPlot(data = point_df, labelStyle = label_style)

  plot
}

extractStyleList <- function(df, prefix) {
  style <- df %>%
    dplyr::select(dplyr::starts_with(prefix)) %>%
    as.list()
  names(style) <- gsub(prefix, "", names(style))
  style
}

#' Get Default Point Style
#'
#' @export
getPointStyle <- function() {
  config()$defaultPointStyle$dataPoints
}

# Get Default Point Style
#
# @param type (character) type of plot to add labels to, either 'ggplot' or 'base'
getLabelStyle <- function(type = c("ggplot", "base")) {
  type <- match.arg(type)

  switch(type,
         "ggplot" = config()$defaultGGText,
         "base" = config()$defaultBaseText)
}

formatPointErrorsOfGGplot <- function(plot, dat = NULL, style = defaultLineFormat(), ...) {
  defaultStyle <- defaultLineFormat()
  requiredElements <- c("size", "linetype", "capwidth", "capheight", "color", "alpha", "hide")

  if (is.null(style)) {
    # if null: take values from config
    style <- defaultStyle
  }

  # check list structure
  if ("dataPoints" %in% names(style)) {
    style <- style[["dataPoints"]]
  }

  # check if all elements are present and complete
  if (!(all(requiredElements %in% names(style)))) {
    missingElements <- setdiff(requiredElements, names(style))
    style[missingElements] <- defaultStyle[missingElements]
  }

  # ensure valid errors
  dat <- dat %>%
    mutate(xmin = ifelse(!is.na(.data$xmin) & .data$xmin > .data$x, .data$x, .data$xmin),
           xmax = ifelse(!is.na(.data$xmax) & .data$xmax < .data$x, .data$x, .data$xmax),
           ymin = ifelse(!is.na(.data$ymin) & .data$ymin > .data$y, .data$y, .data$ymin),
           ymax = ifelse(!is.na(.data$ymax) & .data$ymax < .data$y, .data$y, .data$ymax))

  plot +
    # Horizontal error bars
    geom_errorbarh(data = dat,
                   aes(x = .data$x, y = .data$y, xmin = .data$xmin, xmax = .data$xmax),
                   size = style[["size"]], # thickness
                   height = style[["capheight"]],
                   colour = style[["color"]],
                   linetype = style[["linetype"]],
                   alpha = ifelse(style[["hide"]], 0, style[["alpha"]]),
                   ...) +
    # Vertical error bars
    geom_errorbar(data = dat,
                  aes(x = .data$x, y = .data$y, ymin = .data$ymin, ymax = .data$ymax),
                  size = style[["size"]], # thickness
                  width = style[["capwidth"]],
                  colour = style[["color"]],
                  linetype = style[["linetype"]],
                  alpha = ifelse(style[["hide"]], 0, style[["alpha"]]),
                  ...)
}

#' Point Style Of GGplot
#'
#' Style of points is defined with \code{pointStyle}. Overwrites previous definitions of
#'  \code{geom_point} for the same data.
#'
#' @param plot (ggplot)
#' @param pointStyle (list) named list with style definitions, or output of \code{plotPointsServer}
#' @inheritParams ggplot2::geom_point
#'
#' @export
formatPointsOfGGplot <- function(plot, data = NULL, pointStyle = getPointStyle(), ...) {
  defaultStyle <- getPointStyle()
  requiredElements <- c("symbol", "size", "color", "colorBg", "alpha", "hide")

  if (is.null(pointStyle)) {
    # if null: take values from config
    pointStyle <- defaultStyle
  }

  # check list structure
  if ("dataPoints" %in% names(pointStyle)) {
    pointStyle <- pointStyle[["dataPoints"]]
  }

  # check if all elements are present and complete
  if (!(all(requiredElements %in% names(pointStyle)))) {
    missingElements <- setdiff(requiredElements, names(pointStyle))
    pointStyle[missingElements] <- defaultStyle[missingElements]
  }

  plot +
    geom_point(data = data,
               shape = pointStyle[["symbol"]],
               size = pointStyle[["size"]],
               colour = pointStyle[["color"]],
               fill = pointStyle[["colorBg"]],
               alpha = ifelse(pointStyle[["hide"]], 0, pointStyle[["alpha"]]),
               ...)
}

# Format Point Labels Of GGplot
#
# @param plot (ggplot)
# @param data (data.frame) data with x, y and label information
# @param labelStyle (list) named list with style definitions
# @param ... (list) arguments for \code{geom_text}
formatPointLabelsOfGGPlot <- function(plot, data, labelStyle = getLabelStyle("ggplot"), ...) {
  defaultStyle <- getLabelStyle("ggplot")
  requiredElements <- c("text", "useExpression", "expression", "fontFamily", "fontType", "color", "size", "hide", "angle", "hjust", "vjust")

  if (is.null(labelStyle)) {
    # if null: take values from config
    labelStyle <- defaultStyle
  }

  # check if all elements are present and complete
  if (!(all(requiredElements %in% names(labelStyle)))) {
    missingElements <- setdiff(requiredElements, names(labelStyle))
    labelStyle[missingElements] <- defaultStyle[missingElements]
  }

  # combine data and format for point specific labels
  data_combined <- bind_cols(data[, c("id", "x", "y")], as.data.frame(labelStyle))

  # remove hidden labels
  data_combined <- data_combined[!data_combined$hide, ]

  # split data for expression and text
  data_text <- data_combined[!data_combined$useExpression, ]
  data_expression <- data_combined[data_combined$useExpression, ]

  # plot text labels
  plot <- plot +
    geom_text(data = data_text,
              aes(x = .data$x, y = .data$y, label = .data$text),
              family = data_text$fontFamily, fontface = data_text$fontType,
              color = data_text$color, size = data_text$size,
              angle = data_text$angle, hjust = data_text$hjust, vjust = data_text$vjust,
              show.legend = FALSE,
              ...)

  # plot expression labels (parse = TRUE)
  plot <- plot +
    geom_text(data = data_expression,
              aes(x = .data$x, y = .data$y, label = .data$expression),
              family = data_expression$fontFamily, fontface = data_expression$fontType,
              color = data_expression$color, size = data_expression$size,
              angle = data_expression$angle, hjust = data_expression$hjust, vjust = data_expression$vjust,
              show.legend = FALSE,
              parse = TRUE,
              ...)

  plot
}

# LEGEND ----

#' Legend Style Of GGplot
#'
#' Style of legend is defined with argument \code{legend}. Overwrites previous definitions of \code{theme(legend)}
#'
#' @param plot (ggplot)
#' @param legend (list) named list with style definitions, or output of \code{plotLegendServer}
#' @inheritParams ggplot2::theme
#'
#' @export
formatLegendOfGGplot <- function(plot, legend, scaleFUN = ggplot2::scale_color_manual, ...) {
  # Build the ggplot object
  plot_build <- ggplot_build(plot)

  # Extract plot data
  plot_data <- plot_build$data[[1]]  # Get the first layer's data

  # Get unique color mappings
  unique_colors <- dplyr::distinct(data.frame(
    category = plot_data$colour,   # Extract assigned colors (hex values)
    factor_variable = plot_data$group  # Extract the mapped factor levels
  ))

  # CHECK IF ORDER OF COLORS IS CORRECT <----

  # Dynamically assign extracted colors to factor levels
  color_mapping <- setNames(unique_colors$category, unique(plot_data$group))

  legend_title <- extractTitle(legend$layout$title[[1]], default = names(legend$layout$title))
  legend_labels <- sapply(names(legend$layout$labels), function(name) {
    extractTitle(legend$layout$labels[[name]], default = name)
  }, USE.NAMES = FALSE)

  plot +
    scaleFUN(
      name = legend_title,
      labels = legend_labels,
      values = unname(color_mapping)
    ) +
    theme(legend.position = legend$position,
          legend.title = getElementText(legend$layout$title[[1]]),
          legend.text = getElementText(legend$layout$labels[[1]]),
          ...)
}
