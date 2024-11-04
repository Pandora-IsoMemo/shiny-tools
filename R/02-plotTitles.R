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
                         initText = NULL
                         ) {
  type <- match.arg(type)

  if (is.null(initText)) {
    # if null: take values from config
    initText <- list(
      plotTitle = defaultTextFormat(type = type)[["title"]],
      xAxisText = defaultTextFormat(type = type)[["text"]]
    )
  }

  ns <- NS(id)
  tagList(
    setModuleTitle(title = title, titleTag = titleTag),
    selectInput(
      inputId = ns("labelName"),
      label = "Label",
      choices = c("No label available ..." = ""),
      selected = "plotTitle"
    ),
    checkboxInput(
      inputId = ns("hide"),
      label = "Hide label",
      value = initText[["plotTitle"]][["hide"]],
      width = "100%"
    ),
    conditionalPanel(
      ns = ns,
      condition = "['legendTitle', 'plotTitle', 'xAxisTitle', 'yAxisTitle', 'yAxisTitle2'].includes(input.labelName)",
      checkboxInput(ns("useExpression"),
                    label = "Use mathematical annotation",
                    value = FALSE,
                    width = "100%"),
      conditionalPanel(
        ns = ns,
        condition = "input.useExpression",
        uiOutput(ns("expressionInput")),
        helpText('Example: Use \u003C"Bayesian Estimated" ~ delta^~13~C ~ ("\u2030" - ~ "VPDB")\u003E for \u003C"Bayesian Estimated \u03B4\u00B9\u00B3C (\u2030 - VPDB)"\u003E.', width = "100%"),
        helpText(HTML('Note: "Font type" input is not available for "Expression". For more information, visit the <a href="https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/plotmath.html" target="_blank">R Documentation</a>.'), width = "100%")
      ),
      conditionalPanel(
        ns = ns,
        condition = "!input.useExpression",
        textInput(ns("text"), label = "Text",
                  value = initText[["plotTitle"]][["text"]],
                  placeholder = "Custom title ...")
      ),
    ),
    conditionalPanel(
      ns = ns,
      condition = "!input.useExpression",
      selectInput(
        ns("fontType"),
        label = "Font type",
        choices = fontChoicesSelect(type = type),
        selected = initText[["plotTitle"]][["fontType"]],
        width = "100%")
    ),
    colourInput(ns("color"),
                label = "Text color",
                value = initText[["plotTitle"]][["color"]],
                width = "100%"),
    selectInput(
      inputId = ns("fontFamily"),
      label = "Font family",
      selected = initText[["plotTitle"]][["fontFamily"]],
      choices = availableFonts(),
      width = "100%"
    ),
    sliderInput(
      ns("size"),
      label = "Text size",
      value = initText[["plotTitle"]][["size"]],
      min = sizeValuesSlider(type = type)[["min"]],
      max = sizeValuesSlider(type = type)[["max"]],
      step = sizeValuesSlider(type = type)[["step"]],
      width = "100%"
    ),
    conditionalPanel(
      ns = ns,
      condition = "['xAxisText', 'yAxisText', 'yAxisText2'].includes(input.labelName)",
      sliderInput(
        ns("angle"),
        label = "Text angle",
        value = initText[["xAxisText"]][["angle"]],
        min = 0,
        max = 360,
        step = 5
      ),
      sliderInput(
        ns("hjust"),
        label = "Horizontal adjustment",
        value = initText[["xAxisText"]][["hjust"]],
        min = 0,
        max = 1,
        step = 0.1
      ),
      sliderInput(
        ns("vjust"),
        label = "Vertical adjustment",
        value = initText[["xAxisText"]][["vjust"]],
        min = 0,
        max = 1,
        step = 0.1
      )
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

                 plotText <- reactiveValues()

                 updateSelectInput(session,
                                   "labelName",
                                   choices = availableLabels(availableElements = availableElements))

                 # initialize plotText
                 if (is.null(initText)) {
                   # if null: take values from config
                   for (i in availableLabels(availableElements = availableElements)) {
                     plotText[[i]] <- defaultInitText(type = type,
                                                      availableElements = availableElements)[[i]]
                   }
                 } else if (inherits(initText, "list")) {
                   initText <- validateInitText(initText,
                                                type = type,
                                                availableElements = availableElements)

                   for (i in names(initText)) {
                     plotText[[i]] <- initText[[i]]
                   }
                 } else {
                   plotText <- initText
                 }

                 # if no type available, return plotText
                 if (type == "none") return(plotText)

                 output$expressionInput <- renderUI({
                   if (showParseButton) {
                     fluidRow(
                       column(width = 8,
                              textInput(ns("expression"), label = "Expression",
                                        value = initText[["plotTitle"]][["text"]],
                                        placeholder = "Custom title ...")),
                       column(width = 4,
                              style = "margin-top: 1.7em;",
                              actionButton(ns("parseExpression"), "Parse", width = "100%"))
                     )
                   } else {
                     textInput(ns("expression"), label = "Expression",
                               value = initText[["plotTitle"]][["text"]],
                               placeholder = "Custom title ...")
                   }
                 })

                 observe({
                   req(input[["labelName"]])
                   logDebug("%s: Entering observe 'labelName'", id)

                   # load plotText element inputs of the selected label
                   updateUserInputs(input = input, output = output, session = session,
                                    userInputs = plotText[[input[["labelName"]]]])
                 }) %>%
                   bindEvent(input[["labelName"]])

                 plotText <- observeAndUpdateTextElementsOfLabel(input, output, session, id, plotText, showParseButton)

                 return(plotText)
               })
}

# Observe Text Elements Of Label (no docu for 'man' because it is a helper function)
#
# Observe inputs for different text elements (e.g. color, size, ...) of a selected label (e.g.
# plot title, axis text, ...) and store values in the reactiveValues list 'text'.
#
# @param input input object from server function
# @param output output object from server function
# @param session session from server function
# @param plotText (reactiveValue) contains text elements
# @inheritParams plotTitlesServer
observeAndUpdateTextElementsOfLabel <- function(input, output, session, id, plotText, showParseButton) {
  # set up all observers for text elements
  # we cannot loop over the elements. When looping reactivity gets lost.

  # keep input useExpression for "updateUserInputs()"
  observe({
    req(input[["labelName"]])
    logDebug("%s: Entering observe 'text'", id)

    plotText[[input[["labelName"]]]][["text"]] <- input[["text"]]
  }) %>%
    bindEvent(input[["text"]])

  observe({
    req(input[["labelName"]])
    logDebug("%s: Entering observe 'useExpression'", id)

    plotText[[input[["labelName"]]]][["useExpression"]] <- input[["useExpression"]]
  }) %>%
    bindEvent(input[["useExpression"]])

  if (showParseButton) {
    observe({
      req(input[["labelName"]], input[["parseExpression"]])
      logDebug("%s: Entering observe 'parseExpression'", id)

      plotText[[input[["labelName"]]]][["expression"]] <- input[["expression"]]
    }) %>%
      bindEvent(input[["parseExpression"]])
  } else {
    observe({
      req(input[["labelName"]])
      logDebug("%s: Entering observe 'expression'", id)

      plotText[[input[["labelName"]]]][["expression"]] <- input[["expression"]]
    }) %>%
      bindEvent(input[["expression"]])
  }

  observe({
    req(input[["labelName"]])
    logDebug("%s: Entering observe 'fontFamily'", id)

    plotText[[input[["labelName"]]]][["fontFamily"]] <- input[["fontFamily"]]
  }) %>%
    bindEvent(input[["fontFamily"]])

  observe({
    req(input[["labelName"]])
    logDebug("%s: Entering observe 'fontType'", id)

    plotText[[input[["labelName"]]]][["fontType"]] <- input[["fontType"]]
  }) %>%
    bindEvent(input[["fontType"]])

  observe({
    req(input[["labelName"]])
    logDebug("%s: Entering observe 'color'", id)

    plotText[[input[["labelName"]]]][["color"]] <- input[["color"]]
  }) %>%
    bindEvent(input[["color"]])

  observe({
    req(input[["labelName"]])
    logDebug("%s: Entering observe 'size'", id)

    plotText[[input[["labelName"]]]][["size"]] <- input[["size"]]
  }) %>%
    bindEvent(input[["size"]])

  observe({
    req(input[["labelName"]])
    logDebug("%s: Entering observe 'hide'", id)

    plotText[[input[["labelName"]]]][["hide"]] <- input[["hide"]]
  }) %>%
    bindEvent(input[["hide"]])

  observe({
    req(input[["labelName"]])
    logDebug("%s: Entering observe 'angle'", id)

    plotText[[input[["labelName"]]]][["angle"]] <- input[["angle"]]
  }) %>%
    bindEvent(input[["angle"]])

  observe({
    req(input[["labelName"]])
    logDebug("%s: Entering observe 'hjust'", id)

    plotText[[input[["labelName"]]]][["hjust"]] <- input[["hjust"]]
  }) %>%
    bindEvent(input[["hjust"]])

  observe({
    req(input[["labelName"]])
    logDebug("%s: Entering observe 'vjust'", id)

    plotText[[input[["labelName"]]]][["vjust"]] <- input[["vjust"]]
  }) %>%
    bindEvent(input[["vjust"]])

  return(plotText)
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

# Validate Init Text (no docu for 'man' because it is a helper function)
#
# If elements are missing in initText, add those with default values
#
# @inheritParams plotTitlesServer
# @inheritParams plotExportServer
validateInitText <- function(initText,
                             type = c("none", "ggplot", "base"),
                             availableElements = c("title", "axis")) {
  type <- match.arg(type)

  defaultText <- defaultInitText(type, availableElements = availableElements)

  initText <- initText %>% addMissingElements(defaultText)

  return(initText)
}

addMissingElements <- function(plot_text, default_text) {
  if (!setequal(names(plot_text), names(default_text))) {
    # add missing
    for (i in names(default_text)[!(names(default_text) %in% names(plot_text))]) {
      plot_text[[i]] <- default_text[[i]]
    }

    # order list
    plot_text <- plot_text[names(default_text)]
  }

  plot_text
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
#     output$plot <- renderPlot({
#       testPlotFun() %>%
#         formatTitlesOfGGplot(text = thisTitles) %>%
#         shinyTools::shinyTryCatch(errorTitle = "Plotting failed", alertStyle = "shinyalert")
#     })
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
#           size = 32L,
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
# }
#
# shinyApp(ui = ui, server = server)
