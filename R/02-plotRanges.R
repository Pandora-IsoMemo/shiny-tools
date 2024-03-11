#' Plot Ranges UI
#'
#'
#' @param id module id
#' @inheritParams plotTitlesServer
#'
#' @return tagList
#' @export
plotRangesUI <- function(id, type = c("ggplot", "base")) {
  type <- match.arg(type)

  ns <- NS(id)
  tagList(
    h4("Ranges"),
    selectInput(
      inputId = ns("labelName"),
      label = "Label",
      choices = c(
        "x axis" = "xAxis",
        "y axis" = "yAxis"
      ),
      selected = NA
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
#' @param initRanges (reactiveValues) initial ranges to be used when loading the plot
#'
#' @export
plotRangesServer <- function(id, type = c("none", "ggplot", "base"), initRanges = NULL) {
  type <- match.arg(type)

  moduleServer(id,
               function(input, output, session) {
                 if (!is.null(initRanges)) {
                   ranges <- initRanges
                 } else {
                   ranges <- reactiveValues(
                     xAxis = config()$defaultRange,
                     yAxis = config()$defaultRange
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

                 return(ranges)
               })
}
