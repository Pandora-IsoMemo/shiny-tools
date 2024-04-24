#' Plot Titles UI
#'
#'
#' @param id module id
#' @param title (character) module title
#' @inheritParams plotTitlesServer
#' @inheritParams plotExportServer
#'
#' @return tagList
#' @export
plotTitlesUI <- function(id, title = "Plot Texts", type = c("ggplot", "base"), initText = NULL) {
  type <- match.arg(type)

  if (is.null(initText)) {
    # if null: take values from config
    initText <- list(
      plotTitle = defaultTextFormat(type = type)[["title"]]
    )
  }

  ns <- NS(id)
  tagList(
    h4(title),
    selectInput(
      inputId = ns("labelName"),
      label = "Label",
      choices = c("No label available ..." = ""),
      selected = "plotTitle"
    ),
    conditionalPanel(
      ns = ns,
      condition = "input.labelName == 'legendTitle' | input.labelName == 'plotTitle' | input.labelName == 'xAxisTitle' | input.labelName == 'yAxisTitle'",
      textInput(ns("text"), label = "Text",
                value = initText[["plotTitle"]][["text"]],
                placeholder = "Custom title ..."),
    ),
    colourInput(ns("color"),
                label = "Text color",
                value = initText[["plotTitle"]][["color"]]),
    selectInput(
      ns("fontType"),
      label = "Font type",
      choices = fontChoicesSelect(type = type),
      selected = initText[["plotTitle"]][["fontType"]]),
    sliderInput(
      ns("size"),
      label = "Text size",
      value = initText[["plotTitle"]][["size"]],
      min = sizeValuesSlider(type = type)[["min"]],
      max = sizeValuesSlider(type = type)[["max"]],
      step = sizeValuesSlider(type = type)[["step"]]
    ),
    checkboxInput(
      inputId = ns("hide"),
      label = "Hide label",
      value = initText[["plotTitle"]][["hide"]],
      width = "100%"
    )
  )
}

#' Server function for plot titles
#'
#' Backend for plot titles module
#'
#' @param id namespace id
#' @param type (character) Type of the plot to add titles to, one of "none", "ggplot", "base".
#' @param availableElements (character) set of available labels for specifying the format of text.
#'  May contain elements from \code{c("title", "axis", "legend")}.
#' @inheritParams plotExportServer
#'
#' @export
plotTitlesServer <- function(id,
                             type = c("none", "ggplot", "base"),
                             availableElements = c("title", "axis"),
                             initText = NULL) {
  type <- match.arg(type)
  availableElements <- availableElements %>%
    checkElements()

  moduleServer(id,
               function(input, output, session) {
                 plotText <- reactiveValues()

                 if (is.null(initText)) {
                   # if null: take values from config

                   for (i in availableLabels(availableElements = availableElements)) {
                     plotText[[i]] <- defaultInitText(type = type,
                                                      availableElements = availableElements)[[i]]
                   }
                 } else if (inherits(initText, "list")) {
                   initText <- validateInitText(initText,
                                                type = type,
                                                availableElements = availableElements)

                   for (i in names(initText)) {
                     plotText[[i]] <- initText[[i]]
                   }
                 } else {
                   plotText <- initText
                 }

                 if (type == "none") return(plotText)

                 observe({
                   updateSelectInput(session,
                                     "labelName",
                                     choices = availableLabels(availableElements = availableElements),
                                     selected = availableLabels(availableElements = availableElements)[1])
                 })

                 observe({
                   req(input[["labelName"]])
                   # load plotText element inputs of the selected label
                   updateUserInputs(id, input = input, output = output, session = session,
                                    userInputs = plotText[[input[["labelName"]]]])
                 }) %>%
                   bindEvent(input[["labelName"]])

                 plotText <- observeAndUpdateTextElementsOfLabel(input, output, session, plotText)

                 return(plotText)
               })
}

#' Observe Text Elements Of Label
#'
#' Observe inputs for different text elements (e.g. color, size, ...) of a selected label (e.g.
#' plot title, axis text, ...) and store values in the reactiveValues list 'text'.
#'
#' @param input input object from server function
#' @param output output object from server function
#' @param session session from server function
#' @param plotText (reactiveValue) contains text elements
observeAndUpdateTextElementsOfLabel <- function(input, output, session, plotText) {
  observe({
    req(input[["labelName"]])
    plotText[[input[["labelName"]]]][["text"]] <- input[["text"]]
  }) %>%
    bindEvent(input[["text"]])

  observe({
    req(input[["labelName"]])
    plotText[[input[["labelName"]]]][["fontType"]] <- input[["fontType"]]
  }) %>%
    bindEvent(input[["fontType"]])

  observe({
    req(input[["labelName"]])
    plotText[[input[["labelName"]]]][["color"]] <- input[["color"]]
  }) %>%
    bindEvent(input[["color"]])

  observe({
    req(input[["labelName"]])
    plotText[[input[["labelName"]]]][["size"]] <- input[["size"]]
  }) %>%
    bindEvent(input[["size"]])

  observe({
    req(input[["labelName"]])
    plotText[[input[["labelName"]]]][["hide"]] <- input[["hide"]]
  }) %>%
    bindEvent(input[["hide"]])

  return(plotText)
}

#' Font Choices
#'
#' Mapping of font choices dependent on the plot type
#'
#' @param type (character) plot type, one of "ggplot" or "base"
#'
#' @export
fontChoicesSelect <- function(type = c("ggplot", "base")) {
  type <- match.arg(type)

  switch (type,
          "base" = c("plain text" = 1,
                     "bold face" = 2,
                     "italic" = 3,
                     "bold italic" = 4),
          "ggplot" = c("plain text" = "plain",
                       "bold face" = "bold",
                       "italic" = "italic",
                       "bold italic" = "bold.italic")
  )
}

#' Size Values Slider
#'
#' Initial values for sliderInput title 'size' dependent on the plot type
#'
#' @param type (character) plot type, one of "ggplot" or "base"
sizeValuesSlider  <- function(type = c("ggplot", "base")) {
  type <- match.arg(type)

  switch (type,
          "base" = list(value = 1.2,
                        min = 0.1,
                        max = 10,
                        step = 0.1),
          "ggplot" = list(value = 12,
                          min = 1,
                          max = 30,
                          step = 1)
  )
}

#' Validate Init Text
#'
#' If elements are missing in initText, add those with default values
#'
#' @inheritParams plotTitlesServer
#' @inheritParams plotExportServer
validateInitText <- function(initText,
                             type = c("none", "ggplot", "base"),
                             availableElements = c("title", "axis")) {
  type <- match.arg(type)

  defaultText <- defaultInitText(type, availableElements = availableElements)
  if (!setequal(names(initText), names(defaultText))) {
    # add missing
    for (i in names(defaultText)[!(names(defaultText) %in% names(initText))]) {
      initText[[i]] <- defaultText[[i]]
    }

    # order list
    initText <- initText[names(defaultText)]
  }

  return(initText)
}

#' Default Init Text
#'
#' Initial list with default text elements
#'
#' @inheritParams plotTitlesServer
defaultInitText <- function(type = c("none", "ggplot", "base"),
                            availableElements = c("title", "axis")) {
  type <- match.arg(type)
  thisLabels <- availableLabels(availableElements = availableElements)

  names(entry) <- entry <- thisLabels
  entry[grepl("Title", thisLabels)] <- "title"
  entry[grepl("Text", thisLabels)] <- "text"

  res <- list()
  for (i in thisLabels) {
    res[[i]] <- defaultTextFormat(type = type)[[entry[i]]]
  }

  res
}

#' Default Title Format
#'
#' Initial values for title dependent on the plot type
#'
#' @inheritParams plotTitlesServer
defaultTextFormat <- function(type = c("none", "ggplot", "base")) {
  type <- match.arg(type)

  title <- switch (type,
                   "none" = config()$defaultBaseTitle,
                   "base" = config()$defaultBaseTitle,
                   "ggplot" = config()$defaultGGTitle
  )

  text <- switch (type,
                   "none" = config()$defaultBaseText,
                   "base" = config()$defaultBaseText,
                   "ggplot" = config()$defaultGGText
  )

  list(title = title,
       text = text)
}

availableLabels <- function(availableElements = c("title", "axis")) {
  availableElements <- availableElements %>%
    checkElements()

  config()[["availableElements"]][availableElements] %>%
  unlist() %>%
  keep_deepest_names()
}

checkElements <- function(availableElements) {
  if (!all(availableElements %in% c("title", "axis", "legend")))
    stop(sprintf("Selection of 'availableElements' not allowed. 'availableElements' must be one ore more of c('%s')",
                 paste0(c("title", "axis", "legend"), collapse = "', '")))

  availableElements
}

#' Keep Deepest Names
#'
#' Extracts the names of the deepest level elements from nested names.
#'
#' @param x (vector)  A named vector with named elements and nested names separated by '.'
#'
#' @return A character vector containing the names of the deepest level elements.
keep_deepest_names <- function(x) {
  deepestNames <- names(x) %>%
    sapply(FUN = function(x) gsub(pattern = ".*\\.", replacement = "", x = x), USE.NAMES = FALSE)

  names(x) <- deepestNames
  return(x)
}

# TEST MODULE -------------------------------------------------------------
# To test the module run devtools::load_all() first
# Please comment this code before building the package

# ui <- fluidPage(
#   tagList(
#     navbarPage(
#       header = includeShinyToolsCSS(),
#       title = "test app",
#       theme = shinythemes::shinytheme("flatly"),
#       position = "fixed-top",
#       collapsible = TRUE,
#       id = "test"
#     ),
#     plotOutput("plot"),
#     plotTitlesUI(id = "testMod", type = "ggplot")
#   )
# )
#
# server <- function(input, output, session) {
#     testPlotFun <- function() {
#       data <- data.frame(
#         x = c(1, 2, 3, 4, 5),
#         y = c(2, 4, 1, 7, 3)
#       )
#
#       ggplot2::ggplot(data, ggplot2::aes(x = x, y = y))
#     }
#
#     output$plot <- renderPlot({
#       testPlotFun() %>%
#         formatTitlesOfGGplot(text = thisTitles)
#     })
#
#     thisTitles <- plotTitlesServer(
#       "testMod",
#       type = "ggplot",
#       availableElements = c("title", "axis", "legend"),
#       initText = list(
#         plotTitle = list(
#           text = "testHeader",
#           fontType = "italic",
#           color = "#000000",
#           size = 32L,
#           hide = FALSE
#         ),
#         xAxisTitle = list(
#           text = "test",
#           fontType = "bold",
#           color = "#FF00EA",
#           size = 25,
#           hide = FALSE
#         ),
#         xAxisText = list(
#           fontType = "bold",
#           color = "#FF00EA",
#           size = 25,
#           hide = FALSE
#         ),
#         yAxisTitle = list(
#           text = "",
#           fontType = "plain",
#           color = "#000000",
#           size = 12L,
#           hide = FALSE
#         ),
#         yAxisText = list(
#           fontType = "plain",
#           color = "#000000",
#           size = 12L,
#           hide = FALSE
#         ),
#         legendTitle = list(
#           text = "",
#           fontType = "plain",
#           color = "#000000",
#           size = 12L,
#           hide = FALSE
#         )
#       )
#     )
# }
#
# shinyApp(ui = ui, server = server)
