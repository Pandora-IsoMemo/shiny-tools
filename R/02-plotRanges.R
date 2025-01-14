#' Plot Ranges UI
#'
#'
#' @param id module id
#' @inheritParams setModuleTitle
#' @param initRanges (Deprecated) This parameter is no longer needed and has no effect.
#'
#' @return tagList
#' @export
plotRangesUI <- function(id, title = "Ranges", titleTag = "h4", initRanges = deprecated()) {
  if (lifecycle::is_present(initRanges)) {
    deprecate_warn("24.10.0", "plotRangesUI(initRanges)", details = "This parameter is no longer needed.")
  }

  ns <- NS(id)
  tagList(
    setModuleTitle(title = title, titleTag = titleTag),
    selectInput(
      inputId = ns("labelName"),
      label = "Axis",
      choices = c("x axis" = "xAxis", "y axis" = "yAxis")
    ),
    conditionalPanel(
      ns = ns,
      condition = "input.labelName != 'yAxis2'",
      selectInput(
        inputId = ns("transform"),
        label = "Transformation",
        choices = c(
          "None" = "identity",
          "Logarithmic" = "pseudo-log",
          "Square Root" = "sqrt"#,
          #"reciprocal" = "reciprocal", # transformation leads to issues with axis labels in OsteoBioR
          #"reverse" = "reverse" # transformation leads to issues with axis labels in OsteoBioR
        )
      )
    ),
    # use initRanges from server to setup inputs
    uiOutput(ns("rangesInputs"))
  )
}

#' Server function for plot ranges
#'
#' Backend for plot ranges module
#'
#' @param id namespace id
#' @param type (character) type of the plot to add ranges to, one of "ggplot", "base".
#' @param axes (character) named vector of axes to add ranges to, e.g.
#'  \code{c("x axis" = "xAxis", "y axis" = "yAxis", "2nd y axis" = "yAxis2")}.
#' @inheritParams plotExportServer
#'
#' @export
plotRangesServer <- function(id,
                             type = c("none", "ggplot", "base"),
                             initRanges = NULL,
                             axes = c("x axis" = "xAxis",
                                      "y axis" = "yAxis")) {

  type <- match.arg(type)

  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns

                 ranges <- initializeRanges(session, id, initRanges = initRanges, axes = axes)

                 # return reactiveValues object "ranges" if no type is specified
                 if (type == "none") return(ranges)

                 # set choices for labels
                 updateSelectInput(session, "labelName", choices = axes)

                 # setup range inputs based on initRanges
                 output$rangesInputs <- renderUI({
                   tagList(
                     checkboxInput(
                       inputId = ns("fromData"),
                       label = "Range detected from data",
                       value = ranges[["xAxis"]][["fromData"]],
                       width = "100%"
                     ),
                     conditionalPanel(
                       ns = ns,
                       condition = "!input.fromData",
                       numericInput(
                         ns("min"),
                         label = "Minimum",
                         value = ranges[["xAxis"]][["min"]]
                       ),
                       numericInput(
                         ns("max"),
                         label = "Maximum",
                         value = ranges[["xAxis"]][["max"]]
                       )
                     )
                   )
                 }) %>%
                   bindEvent(input[["labelName"]], once = TRUE)

                 observe({
                   logDebug("%s: Reloading inputs for 'Axis': %s", id, input[["labelName"]])

                   # load range element inputs of the selected label
                   updateUserInputs(input = input, output = output, session = session,
                                    userInputs = ranges[[input[["labelName"]]]])

                   # update label 'fromData' for 2nd y axis
                   if (input[["labelName"]] == "yAxis2") {
                     fromDataLabel <- "Range detected from data (of 1st y axis)"
                   } else {
                     fromDataLabel <- "Range detected from data"
                   }
                   updateCheckboxInput(session, "fromData", label = fromDataLabel)
                 }) %>%
                   bindEvent(input[["labelName"]])

                 ranges <- observeAndUpdateRangeElementsOfLabel(input, output, session, id, ranges)

                 return(ranges)
               })
}

#' Get default ranges
#'
#' @return list
getDefaultRanges <- function() {
  list(xAxis = list(min = 0, max = 1, fromData = TRUE, transform = "identity"),
       yAxis = list(min = 0, max = 1, fromData = TRUE, transform = "identity"),
       yAxis2 = list(min = 0, max = 1, fromData = TRUE, transform = "identity"))
}

# Initialize ranges
#
# Initialize reactiveValues object 'ranges' that will store the user ranges for the axes for
# the plotRanges module. The argument 'axes' specifies the (sub)set of axes to be initialized.
# If 'initRanges' is set, the default ranges are updated with the values from 'initRanges'.
#
# @param session session object from server function
# @param id module id
# @param initRanges (list) optional, named list with range definitions
# @param axes (character) Named vector of axes to add ranges to, e.g. c("x axis" = "xAxis", "y axis" = "yAxis").
#
# @return reactiveValues
initializeRanges <- function(session, id, initRanges, axes) {
  # set default ranges to initialize the "rangesInputs" UI
  # only axes specified in 'getDefaultRanges()' are allowed
  default_ranges <- getDefaultRanges()

  if (!all(axes %in% names(default_ranges))) {
    new_axis <- setdiff(axes, names(default_ranges))
    stop(sprintf("Logic for following axes does not exist yet: '%s'. Please check names of the axes! Following axes are available: '%s'",
                 paste(setdiff(axes, new_axis), collapse = ", "),
                 paste(names(default_ranges), collapse = ", ")
    ))
  }

  # define reactiveValues object 'ranges' to store ranges for set 'axes'
  ranges <- do.call(reactiveValues, default_ranges[axes])

  # update with custom initial ranges if set and if present in default_ranges
  if (!is.null(initRanges) && is.list(initRanges) && !is.reactivevalues(initRanges)) {

    initRanges <- initRanges %>%
      #completeRanges(needed_entries = axes, default_ranges = default_ranges)
      completeValues(choices = axes, default_values = default_ranges)

    ranges <- do.call(reactiveValues, initRanges)
  }

  if (!is.null(initRanges) && is.list(initRanges) && is.reactivevalues(initRanges)) {
    ranges <- initRanges

    # check (once!) if all axes are present, if not use default
    observe({
      logDebug("%s: Checking if all axes are present in user ranges", id)

      new_ranges <- ranges %>%
        #completeRanges(needed_entries = axes, default_ranges = default_ranges)
        completeValues(choices = axes, default_values = default_ranges)

      for (name in names(new_ranges)) {
        ranges[[name]] <- new_ranges[[name]]
      }
    }) %>%
      bindEvent(initRanges$xAxis, once = TRUE)
  }

  return(ranges)
}

# completeRanges <- function(ranges, needed_entries, default_ranges) {
#   # complete ranges with needed entries that are not present in ranges
#   missingEntries <- setdiff(needed_entries, names(ranges))
#
#   # only add default values for missing entries that are present in default_ranges
#   missingEntries <- intersect(missingEntries, names(default_ranges))
#
#   for (name in missingEntries) {
#     ranges[[name]] <- default_ranges[[name]]
#   }
#
#   return(ranges)
# }

# Observe Range Elements Of Label (no docu for 'man' because it is a helper function)
#
# Observe inputs for different text elements (e.g. color, size, ...) of a selected label (e.g.
# plot title, axis text, ...) and store values in the reactiveValues list 'text'.
#
# @param input input object from server function
# @param output output object from server function
# @param session session from server function
# @param ranges (reactiveValue) contains range elements
observeAndUpdateRangeElementsOfLabel <- function(input, output, session, id, ranges) {
  observe({
    req(input[["labelName"]])
    logDebug("%s: Entering observe 'min'", id)

    ranges[[input[["labelName"]]]][["min"]] <- input[["min"]]
    # ensure that min <= max
    maxValue <- max(input[["min"]], input[["max"]], na.rm = TRUE)
    updateNumericInput(session, "max", value = maxValue, min = input[["min"]])
  }) %>%
    bindEvent(input[["min"]])

  observe({
    req(input[["labelName"]])
    logDebug("%s: Entering observe 'max'", id)

    ranges[[input[["labelName"]]]][["max"]] <- input[["max"]]
    # ensure that min <= max
    minValue <- min(input[["min"]], input[["max"]], na.rm = TRUE)
    updateNumericInput(session, "min", value = minValue, max = input[["max"]])
  }) %>%
    bindEvent(input[["max"]])

  observe({
    req(input[["labelName"]])
    logDebug("%s: Entering observe 'fromData'", id)

    ranges[[input[["labelName"]]]][["fromData"]] <- input[["fromData"]]
  }) %>%
    bindEvent(input[["fromData"]])

  observe({
    req(input[["labelName"]])
    logDebug("%s: Entering observe 'transform'", id)

    ranges[[input[["labelName"]]]][["transform"]] <- input[["transform"]]
  }) %>%
    bindEvent(input[["transform"]])

  return(ranges)
}

# TEST MODULE -------------------------------------------------------------
# To test the module run devtools::load_all() first
# Please comment this code before building the package

# ui <- fluidPage(
#   tagList(
#     shinyjs::useShinyjs(),
#     navbarPage(
#       header = includeShinyToolsCSS(),
#       title = "test app",
#       theme = shinythemes::shinytheme("flatly"),
#       position = "fixed-top",
#       collapsible = TRUE,
#       id = "test"
#     ),
#     plotOutput("plot", height = "600px"),
#     plotRangesUI(id = "testMod",
#                  titleTag = "h1")
#   )
# )
#
# server <- function(input, output, session) {
#   testPlotFun <- function() {
#     data <- data.frame(
#       x = c(-10, 2, 3, 4, 5),
#       y = c(1, 2, 3, 4, 50)
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
#                                                                 transform = "identity"),
#                                                    yAxis = list(min = 0,
#                                                                 max = 10,
#                                                                 fromData = TRUE,
#                                                                 transform = "identity"),
#                                                    yAxis2 = list(min = 0,
#                                                                 max = 10,
#                                                                 fromData = TRUE,
#                                                                 transform = "identity")
#                                                    ),
#                                  axes = c("x axis" = "xAxis",
#                                           "y axis" = "yAxis",
#                                           "2nd y axis" = "yAxis2")
#                                  )
#
#   output$plot <- renderPlot({
#     testPlotFun() %>%
#       formatScalesOfGGplot(ranges = thisRanges,
#                            ySecAxisTitle = "title 2nd y axis")
#   })
# }
#
# shinyApp(ui = ui, server = server)
