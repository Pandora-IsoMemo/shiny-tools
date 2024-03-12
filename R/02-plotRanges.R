#' Plot Ranges UI
#'
#'
#' @param id module id
#' @inheritParams plotExportServer
#'
#' @return tagList
#' @export
plotRangesUI <- function(id, initRanges = NULL) {
  if (is.null(initRanges)) {
    # if null: take values from config
    initRanges <- list(
      xAxis = config()$defaultRange,
      yAxis = config()$defaultRange
    )
  }

  ns <- NS(id)
  tagList(
    h4("Ranges"),
    selectInput(
      inputId = ns("labelName"),
      label = "Axis",
      choices = c(
        "x axis" = "xAxis",
        "y axis" = "yAxis"
      ),
      selected = "xAxis"
    ),
    checkboxInput(
      inputId = ns("fromData"),
      label = "From data",
      value = initRanges[["xAxis"]][["fromData"]],
      width = "100%"
    ),
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
                   updateUserInputs(id, input = input, output = output, session = session,
                                    userInputs = ranges[[input[["labelName"]]]])
                 }) %>%
                   bindEvent(input[["labelName"]])

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

                 return(ranges)
               })
}
