#' Plot Titles UI
#'
#'
#' @param id module id
#' @inheritParams plotExportServer
#'
#' @return tagList
#' @export
plotTitlesUI <- function(id, extraPlotFormatting = c("none", "ggplot")) {
  extraPlotFormatting <- match.arg(extraPlotFormatting)

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
    selectInput(
      ns("fontType"),
      label = "Font type",
      choices = fontChoices(type = extraPlotFormatting),
      selected = NULL
    ),
    colourInput(ns("color"), label = "Text color",
                value = defaultTitleFormat()[["color"]]),
    sliderInput(
      ns("size"),
      label = "Text size",
      value = sizeValues(type = extraPlotFormatting)[["value"]],
      min = sizeValues(type = extraPlotFormatting)[["min"]],
      max = sizeValues(type = extraPlotFormatting)[["max"]],
      step = sizeValues(type = extraPlotFormatting)[["step"]]
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
#'
#' @export
plotTitlesServer <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 reactive(
                   setNames(list(
                     list(
                       text = input[["text"]],
                       fontType = input[["fontType"]],
                       color = input[["color"]],
                       size = input[["size"]],
                       hide = input[["hide"]])
                     ), input[["labelName"]])
                 )
               })
}

sizeValues  <- function(type = c("none", "ggplot", "base")) {
  type <- match.arg(type)

  switch (type,
          "none" = list(value = 0.5,
                        min = 0,
                        max = 1,
                        step = 0.1),
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

#' Font Choices
#'
#' @param type (character) plot type, one of "ggplot" or "base"
#'
#' @export
fontChoices <- function(type = c("none", "ggplot", "base")) {
  type <- match.arg(type)

  switch (type,
          "none" = c("no font" = NA),
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
#     plotTitlesUI(id = "testMod", fontChoices = fontChoices(type = "ggplot"))
#   )
# )
#
# server <- function(input, output, session) {
#   plotTitlesServer("testMod")
# }
#
# shinyApp(ui = ui, server = server)
