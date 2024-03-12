#' Plot Ranges UI
#'
#'
#' @param id module id
#'
#' @return tagList
#' @export
plotRangesUI <- function(id) {
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
      selected = NA
    ),
    checkboxInput(
      inputId = ns("fromData"),
      label = "From data",
      value = TRUE,
      width = "100%"
    ),
    numericInput(
      ns("min"),
      label = "Minimum",
      value = 0
    ),
    numericInput(
      ns("max"),
      label = "Maximum",
      value = 1
    )
  )
}

#' Server function for plot ranges
#'
#' Backend for plot ranges module
#'
#' @param id namespace id
#' @param type (character) Type of the plot to add ranges to, one of "ggplot", "base".
#' @param ranges (reactiveValues) initial ranges to be used when loading the plot
#'
#' @export
plotRangesServer <- function(id, type = c("none", "ggplot", "base"), ranges = NULL) {
  type <- match.arg(type)

  moduleServer(id,
               function(input, output, session) {
                 if (is.null(ranges)) {
                   # if null: take values from config
                   ranges <- reactiveValues(
                     xAxis = config()$defaultRange,
                     yAxis = config()$defaultRange
                   )
                 } else if (inherits(ranges, "list")) {
                   # if list: use values to set default values
                   ranges <- reactiveValues(
                     xAxis = ranges[["xAxis"]],
                     yAxis = ranges[["yAxis"]]
                   )
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
