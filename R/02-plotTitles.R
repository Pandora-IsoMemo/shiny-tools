#' Plot Titles UI
#'
#' @inheritParams setModuleTitle
#' @rdname plotTitlesServer
#'
#' @return tagList
#' @export
plotTitlesUI <- function(id,
                         title = "Plot Texts",
                         titleTag = "h4",
                         type = c("ggplot", "base", "none"),
                         initText = NULL) {
  type <- match.arg(type)

  if (is.null(initText)) {
    # if null: take values from config
    initText <- list(plotTitle = defaultTextFormat(type = type)[["title"]],
                     xAxisText = defaultTextFormat(type = type)[["text"]])
  }

  ns <- NS(id)
  tagList(
    setModuleTitle(title = title, titleTag = titleTag),
    selectInput(
      inputId = ns("labelName"),
      label = "Label",
      choices = c("No label available ..." = "")
    ),

    # extracting UI
    formatTextUI(
      ns("text"),
      type = type,
      initTitle = initText[["plotTitle"]],
      initAxis = initText[["xAxisText"]]
    )
  )
}

#' Server function for plot titles
#'
#' Backend for plot titles module
#'
#' @param id namespace id
#' @param type (character) Type of the plot to add titles to, one of "none", "ggplot", "base".
#' @param availableElements (character) set of available labels for specifying the format of text.
#'  May contain elements from \code{c("title", "axis", "yaxis2", "legend")}.
#' @param showParseButton (logical) Show parse button for parsing mathematical expressions.
#' @inheritParams plotExportServer
#'
#' @export
plotTitlesServer <- function(id,
                             type = c("none", "ggplot", "base"),
                             availableElements = c("title", "axis"),
                             showParseButton = TRUE,
                             initText = NULL) {
  type <- match.arg(type)
  availableElements <- availableElements %>%
    checkElements()

  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns

                 updateSelectInput(session,
                                   "labelName",
                                   choices = availableLabels(availableElements = availableElements))

                 plotText <- initializeReactiveObject(
                   session,
                   id,
                   custom_values = initText,
                   choices = availableLabels(availableElements = availableElements),
                   default_fun = defaultInitText,
                   default_fun_args = list(type = type, availableElements = availableElements)
                 )

                 # if no type available, return plotText
                 if (type == "none") return(plotText)

                 init_text <- reactiveVal()
                 observe({
                   logDebug("%s: Entering observe 'input$labelName'", id)

                   if (is.null(input[["labelName"]]) || input[["labelName"]] == "") {
                     init_text(plotText[[names(plotText)[1]]])
                   } else {
                     init_text(plotText[[input[["labelName"]]]])
                   }
                 }) %>%
                   bindEvent(input[["labelName"]])

                 updated_text <- formatTextServer("text",
                                                  init_text = init_text,
                                                  plot_type = c("none", "ggplot", "base"),
                                                  text_type = c("title", "axis"),
                                                  show_parse_button = TRUE,
                                                  label_name = reactive(input[["labelName"]]))

                 observe({
                   logDebug("%s: Entering update plotText", id)
                   req(input[["labelName"]])

                   plotText[[input[["labelName"]]]] <- updated_text() %>%
                     removeHiddenInputs(names(input))
                 }) %>%
                   bindEvent(updated_text())

                 return(plotText)
               })
}

#' Available Fonts
availableFonts <- function() {
  c("sans", "serif", "mono")
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

  switch(type,
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

# Size Values Slider (no docu for 'man' because it is a helper function)
#
# Initial values for sliderInput title 'size' dependent on the plot type
#
# @param type (character) plot type, one of "ggplot" or "base"
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

# Default Init Text (no docu for 'man' because it is a helper function)
#
# Initial list with default text elements
#
# @inheritParams plotTitlesServer
defaultInitText <- function(type = c("none", "ggplot", "base"),
                            availableElements = c("title", "axis")) {
  type <- match.arg(type)
  thisLabels <- availableLabels(availableElements = availableElements)

  names(entry) <- entry <- thisLabels
  entry[grepl("Title", thisLabels)] <- "title"
  entry[grepl("Text", thisLabels)] <- "text"

  res <- list()
  for (i in thisLabels) {
    res[[i]] <- defaultTextFormat(type = type)[[entry[i]]]
  }

  res
}

# Default Title Format (no docu for 'man' because it is a helper function)
#
# Initial values for title dependent on the plot type
#
# @inheritParams plotTitlesServer
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

availableLabels <- function(availableElements = c("title", "axis")) {
  availableElements <- availableElements %>%
    checkElements()

  labelGroups <- defaultLabelGroups()
  labelGroups[availableElements] %>%
  unlist() %>%
  keep_deepest_names()
}

checkElements <- function(availableElements) {
  labelGroups <- defaultLabelGroups()
  configElements <- labelGroups %>% names()

  if (!all(availableElements %in% configElements))
    stop(sprintf("Selection of 'availableElements' not allowed. 'availableElements' must be one ore more of c('%s')",
                 paste0(configElements, collapse = "', '")))

  availableElements
}

# Get default groups of labels that are available in plotTitlesUI under "Label"
#
# @return A list with the default groups of labels
defaultLabelGroups <- function() {
  list(
    title = list(`plot title` = "plotTitle"),
    axis = list(
      `x axis title` = "xAxisTitle",
      `x axis text` = "xAxisText",
      `y axis title` = "yAxisTitle",
      `y axis text` = "yAxisText"
    ),
    yaxis2 = list(`2nd y axis title` = "yAxisTitle2", `2nd y axis text` = "yAxisText2"),
    legend = list(`legend title` = "legendTitle", `legend text` = "legendText")
  )
}

# Keep Deepest Names (no docu for 'man' because it is a helper function)
#
# Extracts the names of the deepest level elements from nested names.
#
# @param x (vector)  A named vector with named elements and nested names separated by '.'
#
# @return A character vector containing the names of the deepest level elements.
keep_deepest_names <- function(x) {
  deepestNames <- names(x) %>%
    sapply(FUN = function(x) gsub(pattern = ".*\\.", replacement = "", x = x), USE.NAMES = FALSE)

  names(x) <- deepestNames
  return(x)
}

removeHiddenInputs <- function(new_text, input_names) {
  # remove "labelName" which is an input but not an entry in new_text
  availableElements <- input_names[input_names != "labelName"]

  # gsub the namespace of the submodule: "text-"
  availableElements <- gsub("text-", "", availableElements)

  # keep all if nothing found (e.g. when initializing the module)
  if (!(any(availableElements %in% names(new_text)))) return(new_text)

  # select only available elements
  new_text[availableElements]
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
#               x = c(1, 2, 3, 4, 5),
#               y = c(3, 5, 2, 8, 7),
#               group = factor(c("A", "B", "A", "B", "A"))
#       )
#
#       ggplot2::ggplot(data, ggplot2::aes(x = x, y = y, color = group)) +
#         ggplot2::geom_line()
#     }
#
#     thisTitles <- plotTitlesServer(
#       "testMod",
#       type = "ggplot",
#       availableElements = c("title", "axis", "yaxis2", "legend"),
#       initText = list(
#         plotTitle = list(
#           text = "testHeader",
#           fontFamily = "sans",
#           fontType = "italic",
#           color = "#000000",
#           size = 15L,
#           hide = FALSE
#         ),
#         xAxisTitle = list(
#           text = "test",
#           fontFamily = "sans",
#           fontType = "bold",
#           color = "#FF00EA",
#           size = 25,
#           hide = FALSE
#         ),
#         xAxisText = list(
#           fontFamily = "sans",
#           fontType = "bold",
#           color = "#FF00EA",
#           size = 25,
#           hide = FALSE,
#           angle = 45,
#           hjust = 1
#         ),
#         yAxisTitle = list(
#           text = "",
#           fontFamily = "sans",
#           fontType = "plain",
#           color = "#000000",
#           size = 12L,
#           hide = FALSE
#         ),
#         yAxisText = list(
#           fontFamily = "sans",
#           fontType = "plain",
#           color = "#000000",
#           size = 12L,
#           hide = FALSE,
#           angle = 0,
#           vjust = 0.5
#         ),
#         legendTitle = list(
#           text = "",
#           fontFamily = "sans",
#           fontType = "plain",
#           color = "#000000",
#           size = 12L,
#           hide = FALSE
#         )
#       )
#     )
#
#     output$plot <- renderPlot({
#       testPlotFun() %>%
#         formatTitlesOfGGplot(text = thisTitles) %>%
#         shinyTools::shinyTryCatch(errorTitle = "Plotting failed", alertStyle = "shinyalert")
#     })
# }
#
# shinyApp(ui = ui, server = server)
