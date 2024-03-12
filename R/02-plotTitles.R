#' Plot Titles UI
#'
#'
#' @param id module id
#' @inheritParams plotTitlesServer
#' @inheritParams plotExportServer
#'
#' @return tagList
#' @export
plotTitlesUI <- function(id, type = c("ggplot", "base"), initTitles = NULL) {
  type <- match.arg(type)

  if (is.null(initTitles)) {
    # if null: take values from config
    initTitles <- list(
      plot = defaultTitleFormat(type = type),
      xAxis = defaultTitleFormat(type = type),
      yAxis = defaultTitleFormat(type = type)
    )
  }

  ns <- NS(id)
  tagList(
    h4("Titles"),
    selectInput(
      inputId = ns("labelName"),
      label = "Label",
      choices = c(
        "plot title" = "plot",
        "x axis" = "xAxis",
        "y axis" = "yAxis"
      ),
      selected = "plot"
    ),
    textInput(ns("text"), label = "Text",
              value = initTitles[["plot"]][["text"]],
              placeholder = "Custom title ..."),
    colourInput(ns("color"),
                label = "Text color",
                value = initTitles[["plot"]][["color"]]),
    selectInput(
      ns("fontType"),
      label = "Font type",
      choices = fontChoicesSelect(type = type),
      selected = initTitles[["plot"]][["fontType"]]),
    sliderInput(
      ns("size"),
      label = "Text size",
      value = initTitles[["plot"]][["size"]],
      min = sizeValuesSlider(type = type)[["min"]],
      max = sizeValuesSlider(type = type)[["max"]],
      step = sizeValuesSlider(type = type)[["step"]]
    ),
    checkboxInput(
      inputId = ns("hide"),
      label = "Hide label",
      value = initTitles[["plot"]][["hide"]],
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
plotTitlesServer <- function(id, type = c("none", "ggplot", "base"), initTitles = NULL) {
  type <- match.arg(type)

  moduleServer(id,
               function(input, output, session) {
                 if (is.null(initTitles)) {
                   # if null: take values from config
                   titles <- reactiveValues(
                     plot = defaultTitleFormat(type = type),
                     xAxis = defaultTitleFormat(type = type),
                     yAxis = defaultTitleFormat(type = type)
                   )
                 } else if (inherits(initTitles, "list")) {
                   titles <- reactiveValues(
                     plot = initTitles[["plot"]],
                     xAxis = initTitles[["xAxis"]],
                     yAxis = initTitles[["yAxis"]]
                   )
                 } else {
                   titles <- initTitles
                 }

                 if (type == "none") return(titles)

                 observe({
                   req(input[["labelName"]])
                   updateUserInputs(id, input = input, output = output, session = session,
                                    userInputs = titles[[input[["labelName"]]]])
                 }) %>%
                   bindEvent(input[["labelName"]])

                 observe({
                   req(input[["labelName"]])
                   titles[[input[["labelName"]]]][["text"]] <- input[["text"]]
                 }) %>%
                   bindEvent(input[["text"]])

                 observe({
                   req(input[["labelName"]])
                   titles[[input[["labelName"]]]][["fontType"]] <- input[["fontType"]]
                 }) %>%
                   bindEvent(input[["fontType"]])

                 observe({
                   req(input[["labelName"]])
                   titles[[input[["labelName"]]]][["color"]] <- input[["color"]]
                 }) %>%
                   bindEvent(input[["color"]])

                 observe({
                   req(input[["labelName"]])
                   titles[[input[["labelName"]]]][["size"]] <- input[["size"]]
                 }) %>%
                   bindEvent(input[["size"]])

                 observe({
                   req(input[["labelName"]])
                   titles[[input[["labelName"]]]][["hide"]] <- input[["hide"]]
                 }) %>%
                   bindEvent(input[["hide"]])

                 return(titles)
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

#' Default Title Format
#'
#' Initial values for title dependent on the plot type
#'
#' @param type (character) plot type, one of "ggplot" or "base"
defaultTitleFormat <- function(type = c("none", "ggplot", "base")) {
  type <- match.arg(type)

  switch (type,
          "none" = config()$defaultBaseTitle,
          "base" = config()$defaultBaseTitle,
          "ggplot" = config()$defaultGGTitle
  )
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
#     plotTitlesUI(id = "testMod", type = "ggplot")
#   )
# )
#
# server <- function(input, output, session) {
#   plotTitlesServer("testMod",
#                    type = "ggplot",
#                    titles = list(plot = list(text = "testHeader", fontType = "italic", color = "#000000",
#                                              size = 32L, hide = FALSE),
#                                  xAxis = list(text = "test", fontType = "bold",
#                                               color = "#FF00EA", size = 25, hide = FALSE),
#                                  yAxis = list(text = "", fontType = "plain", color = "#000000",
#                                               size = 12L, hide = FALSE)))
# }
#
# shinyApp(ui = ui, server = server)
