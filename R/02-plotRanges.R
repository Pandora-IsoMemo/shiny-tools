#' Plot Ranges UI
#'
#'
#' @param id module id
#' @inheritParams setModuleTitle
#' @inheritParams plotExportServer
#'
#' @return tagList
#' @export
plotRangesUI <- function(id, title = "Ranges", titleTag = "h4", initRanges = NULL) {
  if (is.null(initRanges)) {
    # if null: take values from config
    initRanges <- list(
      xAxis = config()$defaultRange,
      yAxis = config()$defaultRange
    )
  }

  ns <- NS(id)
  tagList(
    setModuleTitle(title = title, titleTag = titleTag),
    selectInput(
      inputId = ns("labelName"),
      label = "Axis",
      choices = c(
        "x axis" = "xAxis",
        "y axis" = "yAxis"
      ),
      selected = "xAxis"
    ),
    selectInput(
      inputId = ns("trans"),
      label = "Transformation",
      choices = list(
        `No transformation` = c(
        "identity" = "identity"),
        `Logarithmic transformation` = c(
          "log10" = "log10",
          "log2" = "log2",
          "log" = "log"),
        `Power transformation` = c(
          "sqrt" = "sqrt",
          "reciprocal" = "reciprocal"),
        `Reverse transformation` = c(
        "reverse" = "reverse")
      )
    ),
    checkboxInput(
      inputId = ns("fromData"),
      label = "Range detected from data",
      value = initRanges[["xAxis"]][["fromData"]],
      width = "100%"
    ),
    conditionalPanel(
      ns = ns,
      condition = "!input.fromData",
      numericInput(
        ns("min"),
        label = "Minimum",
        value = initRanges[["xAxis"]][["min"]]
      ),
      numericInput(
        ns("max"),
        label = "Maximum",
        value = initRanges[["xAxis"]][["max"]]
      )
    )
  )
}

#' Server function for plot ranges
#'
#' Backend for plot ranges module
#'
#' @param id namespace id
#' @param type (character) Type of the plot to add ranges to, one of "ggplot", "base".
#' @inheritParams plotExportServer
#'
#' @export
plotRangesServer <- function(id, type = c("none", "ggplot", "base"), initRanges = NULL) {
  type <- match.arg(type)

  moduleServer(id,
               function(input, output, session) {
                 if (is.null(initRanges)) {
                   # if null: take values from config
                   ranges <- reactiveValues(
                     xAxis = config()$defaultRange,
                     yAxis = config()$defaultRange
                   )
                 } else if (inherits(initRanges, "list")) {
                   # if list: use values to set default values
                   ranges <- reactiveValues(
                     xAxis = initRanges[["xAxis"]],
                     yAxis = initRanges[["yAxis"]]
                   )
                 } else {
                   ranges <- initRanges
                 }

                 if (type == "none") return(ranges)

                 observe({
                   # load range element inputs of the selected label
                   updateUserInputs(id, input = input, output = output, session = session,
                                    userInputs = ranges[[input[["labelName"]]]])
                 }) %>%
                   bindEvent(input[["labelName"]])

                 ranges <- observeAndUpdateRangeElementsOfLabel(input, output, session, ranges)

                 return(ranges)
               })
}

#' Observe Range Elements Of Label
#'
#' Observe inputs for different text elements (e.g. color, size, ...) of a selected label (e.g.
#' plot title, axis text, ...) and store values in the reactiveValues list 'text'.
#'
#' @param input input object from server function
#' @param output output object from server function
#' @param session session from server function
#' @param ranges (reactiveValue) contains range elements
observeAndUpdateRangeElementsOfLabel <- function(input, output, session, ranges) {
  observe({
    req(input[["labelName"]])
    ranges[[input[["labelName"]]]][["min"]] <- input[["min"]]
    # ensure that min <= max
    maxValue <- max(input[["min"]], input[["max"]], na.rm = TRUE)
    updateNumericInput(session, "max", value = maxValue, min = input[["min"]])
  }) %>%
    bindEvent(input[["min"]])

  observe({
    req(input[["labelName"]])
    ranges[[input[["labelName"]]]][["max"]] <- input[["max"]]
    # ensure that min <= max
    minValue <- min(input[["min"]], input[["max"]], na.rm = TRUE)
    updateNumericInput(session, "min", value = minValue, max = input[["max"]])
  }) %>%
    bindEvent(input[["max"]])

  observe({
    req(input[["labelName"]])
    ranges[[input[["labelName"]]]][["fromData"]] <- input[["fromData"]]
  }) %>%
    bindEvent(input[["fromData"]])

  observe({
    req(input[["labelName"]])
    ranges[[input[["labelName"]]]][["trans"]] <- input[["trans"]]
  }) %>%
    bindEvent(input[["trans"]])

  return(ranges)
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
#     plotRangesUI(id = "testMod",
#                  titleTag = "h1",
#                  initRanges = list(xAxis = list(min = 0, max = 10, fromData = FALSE),
#                                    yAxis = list(min = 0, max = 10, fromData = TRUE)))
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
#     ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) +
#       ggplot2::geom_point()
#   }
#
#   thisRanges <- plotRangesServer("testMod",
#                                  type = "ggplot",
#                                  initRanges = list(xAxis = list(min = 0,
#                                                                 max = 10,
#                                                                 fromData = FALSE,
#                                                                 trans = "identity"),
#                                                  yAxis = list(min = 0,
#                                                               max = 10,
#                                                               fromData = TRUE,
#                                                               trans = "identity")))
#
#   output$plot <- renderPlot({
#     testPlotFun() %>%
#       formatRangesOfGGplot(ranges = thisRanges)
#   })
# }
#
# shinyApp(ui = ui, server = server)
