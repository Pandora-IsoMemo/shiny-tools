#' Plot Titles UI
#'
#'
#' @param id module id
#' @inheritParams plotTitlesServer
#'
#' @return tagList
#' @export
plotTitlesUI <- function(id, type = c("ggplot", "base")) {
  type <- match.arg(type)

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
      selected = NA
    ),
    textInput(ns("text"), label = "Text", value = NULL),
    colourInput(ns("color"), label = "Text color",
                value = defaultTitleFormat(type = type)[["color"]]),
    selectInput(
      ns("fontType"),
      label = "Font type",
      choices = fontChoicesSelect(type = type),
      selected = NULL
    ),
    sliderInput(
      ns("size"),
      label = "Text size",
      value = sizeValuesSlider(type = type)[["value"]],
      min = sizeValuesSlider(type = type)[["min"]],
      max = sizeValuesSlider(type = type)[["max"]],
      step = sizeValuesSlider(type = type)[["step"]]
    ),
    checkboxInput(
      inputId = ns("hide"),
      label = "Hide label",
      value = FALSE,
      width = "100%"
    )
  )
}

#' Server function for plot titles
#'
#' Backend for plot titles module
#'
#' @param id namespace id
#' @param type (character) Type of the plot to add titles to, one of "ggplot", "base".
#' @param titles (reactiveValues) initial titles to be used when loading the plot
#'
#' @export
plotTitlesServer <- function(id, type = c("none", "ggplot", "base"), titles = NULL) {
  type <- match.arg(type)

  moduleServer(id,
               function(input, output, session) {
                 if (is.null(titles)) {
                   titles <- reactiveValues(
                     plot = defaultTitleFormat(type = type),
                     xAxis = defaultTitleFormat(type = type),
                     yAxis = defaultTitleFormat(type = type)
                   )
                 }

                 if (type == "none") return(titles)

                 observe({
                   updateUserInputs(id, input = input, output = output, session = session,
                                    userInputs = titles[[input[["labelName"]]]])
                 }) %>%
                   bindEvent(input[["labelName"]])

                 observe({
                   req(input[["labelName"]])
                   titles[[input[["labelName"]]]] <- list(text = input[["text"]],
                                                          fontType = input[["fontType"]],
                                                          color = input[["color"]],
                                                          size = input[["size"]],
                                                          hide = input[["hide"]])
                 }) %>%
                   bindEvent(list(input[["text"]],
                                  input[["fontType"]],
                                  input[["color"]],
                                  input[["size"]],
                                  input[["hide"]]))

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
                        max = 5,
                        step = 0.1),
          "ggplot" = list(value = 12,
                          min = 1,
                          max = 20,
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
#   plotTitlesServer("testMod")
# }
#
# shinyApp(ui = ui, server = server)
