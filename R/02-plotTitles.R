#' Plot Titles UI
#'
#'
#' @param id module id
#' @inheritParams plotTitlesServer
#' @inheritParams plotExportServer
#'
#' @return tagList
#' @export
plotTitlesUI <- function(id, type = c("ggplot", "base"), initText = NULL) {
  type <- match.arg(type)

  if (is.null(initText)) {
    # if null: take values from config
    initText <- list(
      plotTitle = defaultTextFormat(type = type)[["title"]]
    )
  }

  ns <- NS(id)
  tagList(
    h4("Plot Texts"),
    selectInput(
      inputId = ns("labelName"),
      label = "Label",
      choices = c(
        "plot title" = "plotTitle",
        "x axis title" = "xAxisTitle",
        "y axis title" = "yAxisTitle",
        "x axis text" = "xAxisText",
        "y axis text" = "yAxisText"
      ),
      selected = "plotTitle"
    ),
    conditionalPanel(
      ns = ns,
      condition = "input.labelName == 'plotTitle' | input.labelName == 'xAxisTitle' | input.labelName == 'yAxisTitle'",
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
#' @inheritParams plotExportServer
#'
#' @export
plotTitlesServer <- function(id, type = c("none", "ggplot", "base"), initText = NULL) {
  type <- match.arg(type)

  moduleServer(id,
               function(input, output, session) {
                 if (is.null(initText)) {
                   # if null: take values from config
                   text <- reactiveValues(
                     plotTitle = defaultTextFormat(type = type)[["title"]],
                     xAxisTitle = defaultTextFormat(type = type)[["title"]],
                     yAxisTitle = defaultTextFormat(type = type)[["title"]],
                     xAxisText = defaultTextFormat(type = type)[["text"]],
                     yAxisText = defaultTextFormat(type = type)[["text"]]
                   )
                 } else if (inherits(initText, "list")) {
                   initText <- validateInitText(initText)

                   text <- reactiveValues(
                     plotTitle = initText[["plotTitle"]],
                     xAxisTitle = initText[["xAxisTitle"]],
                     yAxisTitle = initText[["yAxisTitle"]],
                     xAxisText = initText[["xAxisText"]],
                     yAxisText = initText[["yAxisText"]]
                   )
                 } else {
                   text <- initText
                 }

                 if (type == "none") return(text)

                 observe({
                   req(input[["labelName"]])
                   updateUserInputs(id, input = input, output = output, session = session,
                                    userInputs = text[[input[["labelName"]]]])
                 }) %>%
                   bindEvent(input[["labelName"]])

                 observe({
                   req(input[["labelName"]])
                   text[[input[["labelName"]]]][["text"]] <- input[["text"]]
                 }) %>%
                   bindEvent(input[["text"]])

                 observe({
                   req(input[["labelName"]])
                   text[[input[["labelName"]]]][["fontType"]] <- input[["fontType"]]
                 }) %>%
                   bindEvent(input[["fontType"]])

                 observe({
                   req(input[["labelName"]])
                   text[[input[["labelName"]]]][["color"]] <- input[["color"]]
                 }) %>%
                   bindEvent(input[["color"]])

                 observe({
                   req(input[["labelName"]])
                   text[[input[["labelName"]]]][["size"]] <- input[["size"]]
                 }) %>%
                   bindEvent(input[["size"]])

                 observe({
                   req(input[["labelName"]])
                   text[[input[["labelName"]]]][["hide"]] <- input[["hide"]]
                 }) %>%
                   bindEvent(input[["hide"]])

                 return(text)
               })
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
#' @inheritParams plotTitleServer
#' @inheritParams plotExportServer
validateInitText <- function(initText, type = c("none", "ggplot", "base")) {
  type <- match.arg(type)

  if (!setequal(names(initText), names(defaultInitText(type)))) {
    # add missing
    for (i in names(defaultInitText(type))[!(names(defaultInitText(type)) %in% names(initText))]) {
      initText[[i]] <- defaultInitText(type)[[i]]
    }

    # order list
    initText <- initText[names(defaultInitText(type))]
  }

  return(initText)
}

#' Default Init Text
#'
#' Initial list with default text elements
#'
#' @inheritParams plotTitleServer
defaultInitText <- function(type = c("none", "ggplot", "base")) {
  type <- match.arg(type)

  list(
    plotTitle = defaultTextFormat(type = type)[["title"]],
    xAxisTitle = defaultTextFormat(type = type)[["title"]],
    yAxisTitle = defaultTextFormat(type = type)[["title"]],
    xAxisText = defaultTextFormat(type = type)[["text"]],
    yAxisText = defaultTextFormat(type = type)[["text"]]
  )
}

#' Default Title Format
#'
#' Initial values for title dependent on the plot type
#'
#' @inheritParams plotTitleServer
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
#   thisTitles <- plotTitlesServer("testMod",
#                                  type = "ggplot",
#                                  initText = list(plotTitle = list(text = "testHeader", fontType = "italic", color = "#000000",
#                                                                   size = 32L, hide = FALSE),
#                                                  xAxisTitle = list(text = "test", fontType = "bold",
#                                                                    color = "#FF00EA", size = 25, hide = FALSE),
#                                                  yAxisTitle = list(text = "", fontType = "plain", color = "#000000",
#                                                                    size = 12L, hide = FALSE),
#                                                  xAxisText = list(fontType = "bold",
#                                                                   color = "#FF00EA", size = 25, hide = FALSE),
#                                                  yAxisText = list(fontType = "plain", color = "#000000",
#                                                                   size = 12L, hide = FALSE)))
# }
#
# shinyApp(ui = ui, server = server)
