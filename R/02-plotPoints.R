#' Plot Points UI
#'
#'
#' @param id module id
#' @inheritParams setModuleTitle
#' @inheritParams plotPointsServer
#'
#' @return tagList
#' @export
plotPointsUI <- function(id, title = "Data Points", titleTag = "h4", type = c("ggplot", "base"), initStyle = NULL) {
  type <- match.arg(type)
  if (is.null(initStyle)) {
    # if null: take values from config
    initStyle <- config()$defaultPointStyle
  }

  ns <- NS(id)
  tagList(
    setModuleTitle(title = title, titleTag = titleTag),
    checkboxInput(
      inputId = ns("hide"),
      label = "Hide",
      value = initStyle[["dataPoints"]][["hide"]],
      width = "100%"
    ),
    selectInput(
      inputId = ns("symbol"),
      label = "Symbol",
      choices = symbolChoicesSelect(),
      selected = initStyle[["dataPoints"]][["symbol"]],
      width = "100%"
    ),
    colourInput(
      inputId = ns("color"),
      label = "Color",
      value = initStyle[["dataPoints"]][["color"]],
      width = "100%"
    ),
    sliderInput(
      inputId = ns("size"),
      label = "Size",
      min = 0,
      max = 20,
      value = initStyle[["dataPoints"]][["size"]],
      width = "100%"
    ),
    sliderInput(
      inputId = ns("alpha"),
      label = "Opacity",
      min = 0,
      max = 1,
      value = initStyle[["dataPoints"]][["alpha"]],
      width = "100%"
    ),
    conditionalPanel(
      condition = "input.symbol == 21 | input.symbol == 22 |input.symbol == 23 |input.symbol == 24 |input.symbol == 25",
      ns = ns,
      colourInput(
        inputId = ns("colorBg"),
        label = "Background color",
        value = initStyle[["dataPoints"]][["colorBg"]],
        width = "100%"
      ),
      if (type == "base") {
        sliderInput(
          inputId = ns("lineWidthBg"),
          label = "Thickness",
          min = 0,
          max = 20,
          value = initStyle[["dataPoints"]][["lineWidthBg"]],
          width = "100%"
        )
      } else NULL
    )
  )
}

#' Server function for plot points
#'
#' Backend for plot points module
#'
#' @param id namespace id
#' @param type (character) Type of the plot to edit points for, one of "ggplot", "base".
#' @param hideInput (character) inputs that should be disabled (hidden) when applying this module.
#'  Possible inputs are "hide", "symbol", "color", "size", "alpha", "colorBg", "lineWidthBg".
#'  Please use \code{shinyjs::useShinyjs()} in your UI function to enable this feature.
#' @param initStyle (list) optional, named list with style definitions, should have the same format
#'  as the default output of \code{plotPointsServer}
#' @param reloadInit (reactiveVal) logical, should \code{initStyle} be reloaded?
#'
#' @export
plotPointsServer <- function(id, type = c("ggplot", "base"), hideInput = c(), initStyle = NULL, reloadInit = reactiveVal(FALSE)) {
  type <- match.arg(type)
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns

                 style <- custom_style <- initializeReactiveObject(
                   session,
                   id,
                   custom_values = initStyle,
                   choices = c("dataPoints"),
                   default_fun = defaultDataPointStyle
                 )

                 observe({
                   req(length(hideInput) > 0)
                   logDebug("%s: Entering observe 'hideInput'", id)

                   # hide inputs
                   for (i in hideInput) {
                     shinyjs::hide(ns(i), asis = TRUE)
                   }
                 })

                 observe({
                   req(isTRUE(reloadInit()))
                   logDebug("%s: Entering reload 'initStyle'", id)

                   if (is.reactive(initStyle)) {
                     new_inputs <- initStyle()[["dataPoints"]]
                   } else {
                     new_inputs <- custom_style[["dataPoints"]]
                   }

                   updateUserInputs(
                     input = input,
                     output = output,
                     session = session,
                     userInputs = new_inputs
                   )
                 }) %>%
                   bindEvent(reloadInit())

                 style <- observeAndUpdatePointElements(input, output, session, id,
                                                        style = style,
                                                        type = type)

                 return(style)
               })
}


# Observe Point Elements
#
# Observe inputs for different text elements (e.g. color, size, ...) of a selected label (e.g.
# plot title, axis text, ...) and store values in the reactiveValues list 'text'.
#
# @param input input object from server function
# @param output output object from server function
# @param session session from server function
# @param style (reactiveValue) contains point elements
# @inheritParams plotPointsServer
observeAndUpdatePointElements <- function(input, output, session, id, style, type) {
  observe({
    logDebug("%s: Entering observe 'symbol'", id)

    style[["dataPoints"]][["symbol"]] <- as.numeric(input[["symbol"]])
  }) %>%
    bindEvent(input[["symbol"]])

  observe({
    logDebug("%s: Entering observe 'color'", id)

    style[["dataPoints"]][["color"]] <- input[["color"]]
  }) %>%
    bindEvent(input[["color"]])

  observe({
    logDebug("%s: Entering observe 'colorBg'", id)

    style[["dataPoints"]][["colorBg"]] <- input[["colorBg"]]
  }) %>%
    bindEvent(input[["colorBg"]])

  observe({
    logDebug("%s: Entering observe 'size'", id)

    style[["dataPoints"]][["size"]] <- input[["size"]]
  }) %>%
    bindEvent(input[["size"]])

  observe({
    logDebug("%s: Entering observe 'alpha'", id)

    style[["dataPoints"]][["alpha"]] <- input[["alpha"]]
  }) %>%
    bindEvent(input[["alpha"]])

  observe({
    logDebug("%s: Entering observe 'hide'", id)

    style[["dataPoints"]][["hide"]] <- input[["hide"]]
  }) %>%
    bindEvent(input[["hide"]])

  if (type == "base") {
    observe({
      logDebug("%s: Entering observe 'lineWidthBg'", id)

      style[["dataPoints"]][["lineWidthBg"]] <- input[["lineWidthBg"]]
    }) %>%
      bindEvent(input[["lineWidthBg"]])
  }

  return(style)
}

defaultDataPointStyle <- function() {
  config()$defaultPointStyle["dataPoints"]
}

#' Font Choices
#'
#' Mapping of font choices to numeric values used in \code{ggplot2::geom_point} or in \code{base::points()}.
#'
#' @export
symbolChoicesSelect <- function() {
  c("square" = 0,
    "circle" = 1,
    "triangle point up" = 2,
    "plus" = 3,
    "cross" = 4,
    "diamond" = 5,
    "triangle point down" = 6,
    "square cross" = 7,
    "star" = 8,
    "diamond plus" = 9,
    "circle plus" = 10,
    "triangles up and down" = 11,
    "square plus" = 12,
    "circle cross" = 13,
    "square and triangle down" = 14,
    "filled square" = 15,
    "filled circle" = 16,
    "filled triangle point-up" = 17,
    "filled diamond" = 18,
    "solid circle" = 19,
    "bullet (smaller circle)" = 20,
    "filled circle bg color" = 21,
    "filled square bg color" = 22,
    "filled diamond bg color" = 23,
    "filled triangle point-up bg color" = 24,
    "filled triangle point-down bg color" = 25)
}


# TEST MODULE -------------------------------------------------------------
# To test the module run devtools::load_all() first
# Please comment this code before building the package

# testStyle <- function() {
#   list(dataPoints = list(symbol = 23, color = "#00FF22", colorBg = "#FF00EA",
#                                     size = 8, alpha = 0.3, lineWidthBg = 1, hide = FALSE))
# }
#
# ui <- fluidPage(shinyjs::useShinyjs(),
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
#     plotPointsUI(id = "testMod", type = "ggplot", initStyle = testStyle())
#   )
# )
#
# server <- function(input, output, session) {
#   testPlotFun <- function() {
#     data <- data.frame(
#       x = c(1, 2, 3, 4, 5),
#       y = c(2, 4, 1, 7, 3)
#     )
#
#     ggplot2::ggplot(data, ggplot2::aes(x = x, y = y))
#   }
#
#   thisStyle <- plotPointsServer(id = "testMod",
#                                 type = "ggplot",
#                                 initStyle = testStyle(),
#                                 hideInput = c("color"))
#
#   output$plot <- renderPlot({
#     testPlotFun() %>%
#       formatPointsOfGGplot(pointStyle = thisStyle)
#   })
# }
#
# shinyApp(ui = ui, server = server)
