# Text Format UI
#
# @param id id of parent(!) module
# @rdname plotTitlesServer
#
# @return tagList
formatTextUI <- function(id,
                         type = c("ggplot", "base", "none"),
                         initStyle = NULL,
                         layout_info_text = NULL,
                         width = "100%") {
  type <- match.arg(type)

  if (is.null(initStyle)) {
    # if null: take values from config
    initStyle <- defaultTextFormat(type = type)[["title"]]
  }

  # complete initStyle
  initStyle <- initStyle %>%
    completeValues(default_values = defaultTextFormat(type = type)[["title"]])

  ns <- NS(id)
  tagList(
    conditionalPanel(
      ns = ns,
      condition = "output.show_text_ui",
      checkboxInput(
        ns("useExpression"),
        label = "Use mathematical annotation",
        value = FALSE,
        width = width
      ),
      conditionalPanel(
        ns = ns,
        condition = "input.useExpression",
        uiOutput(ns("expressionInput")),
        helpText(
          'Example: Use \u003C"Bayesian Estimated" ~ delta^~13~C ~ ("\u2030" - ~ "VPDB")\u003E for \u003C"Bayesian Estimated \u03B4\u00B9\u00B3C (\u2030 - VPDB)"\u003E.',
          width = width
        ),
        helpText(
          HTML(
            'Note: "Font type" input is not available for "Expression". For more information, visit the <a href="https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/plotmath.html" target="_blank">R Documentation</a>.'
          ),
          width = width
        )
      ),
      conditionalPanel(
        ns = ns,
        condition = "!input.useExpression",
        textInput(
          ns("text"),
          label = "Text",
          value = initStyle[["text"]],
          placeholder = "Custom text ...",
          width = width
        )
      ),
    ),
    if (!is.null(layout_info_text)) helpText(
      layout_info_text,
      width = width
    ) else NULL,
    checkboxInput(
      inputId = ns("hide"),
      label = "Hide",
      value = initStyle[["hide"]],
      width = width
    ),
    conditionalPanel(
      ns = ns,
      condition = "!input.useExpression",
      selectInput(
        ns("fontType"),
        label = "Font type",
        choices = fontChoicesSelect(type = type),
        selected = initStyle[["fontType"]],
        width = width
      )
    ),
    colourInput(
      ns("color"),
      label = "Text color",
      value = initStyle[["color"]],
      width = width
    ),
    selectInput(
      inputId = ns("fontFamily"),
      label = "Font family",
      selected = initStyle[["fontFamily"]],
      choices = availableFonts(),
      width = width
    ),
    sliderInput(
      ns("size"),
      label = "Text size",
      value = initStyle[["size"]],
      min = sizeValuesSlider(type = type)[["min"]],
      max = sizeValuesSlider(type = type)[["max"]],
      step = sizeValuesSlider(type = type)[["step"]],
      width = width
    ),
    conditionalPanel(
      ns = ns,
      condition = "output.show_position_ui",
      sliderInput(
        ns("angle"),
        label = "Text angle",
        value = initStyle[["angle"]],
        min = 0,
        max = 360,
        step = 5,
        width = width
      ),
      sliderInput(
        ns("hjust"),
        label = "Horizontal adjustment",
        value = initStyle[["hjust"]],
        min = -5,
        max = 5,
        step = 0.1,
        width = width
      ),
      sliderInput(
        ns("vjust"),
        label = "Vertical adjustment",
        value = initStyle[["vjust"]],
        min = -5,
        max = 5,
        step = 0.1,
        width = width
      )
    )
  )
}

# Server function for plot titles
#
# Backend for plot titles module
#
# @param id namespace id
# @param show_parse_button (logical) Show parse button for parsing mathematical expressions.
#  This should be FALSE if there is an 'apply' button in the parent module
# @inheritParams plotExportServer
formatTextServer <- function(id,
                             init_layout = reactive(defaultInitTitle()),
                             show_parse_button = TRUE,
                             element_id = reactive("plotTitle"),
                             text_inputs = c("element_id", "show", "hide"),
                             position_inputs = c("element_id", "show", "hide"),
                             width = "100%") {
  text_inputs <- match.arg(text_inputs)
  position_inputs <- match.arg(position_inputs)
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    new_text <- reactiveValues()

    observe({
      logDebug("%s: Entering observe 'init_layout'", id)
      for (name in names(init_layout())) {
        new_text[[name]] <- init_layout()[[name]]
      }
    }) %>%
      bindEvent(init_layout())

    # Bind the reactive value to an output for the conditionalPanel
    output$show_text_ui <- reactive({
      switch(
        text_inputs,
        "element_id" = showTextInputs(element_id()),
        "show" = TRUE,
        "hide" = FALSE
      )
    })

    output$show_position_ui <- reactive({
      switch(
        position_inputs,
        "element_id" = showPositionInputs(element_id()),
        "show" = TRUE,
        "hide" = FALSE
      )
    })

    # Mark the output as usable in conditionalPanel
    outputOptions(output, "show_text_ui", suspendWhenHidden = FALSE)
    outputOptions(output, "show_position_ui", suspendWhenHidden = FALSE)

    output$expressionInput <- renderUI({
      if (show_parse_button) {
        fluidRow(
          column(
            width = 8,
            textInput(
              ns("expression"),
              label = "Expression",
              value = init_layout()[["text"]],
              placeholder = "Custom title ..."
            )
          ),
          column(
            width = 4,
            style = "margin-top: 1.7em;",
            actionButton(ns("parseExpression"), "Parse", width = width)
          )
        )
      } else {
        textInput(
          ns("expression"),
          label = "Expression",
          value = init_layout()[["expression"]],
          placeholder = "Custom title ..."
        )
      }
    })

    observe({
      req(element_id())
      logDebug("%s: Entering observe 'element_id'", id)

      updateUserInputs(
        input = input,
        output = output,
        session = session,
        userInputs = init_layout()
      )
    }) %>%
      bindEvent(element_id())

    new_text <- observeAndUpdateTextElements(input, output, session, id, new_text, show_parse_button)

    return(reactive(reactiveValuesToList(new_text)))
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

  switch(
    type,
    "base" = c(
      "plain text" = 1,
      "bold face" = 2,
      "italic" = 3,
      "bold italic" = 4
    ),
    "ggplot" = c(
      "plain text" = "plain",
      "bold face" = "bold",
      "italic" = "italic",
      "bold italic" = "bold.italic"
    )
  )
}

# Size Values Slider (no docu for 'man' because it is a helper function)
#
# Initial values for sliderInput title 'size' dependent on the plot type
#
# @param type (character) plot type, one of "ggplot" or "base"
sizeValuesSlider  <- function(type = c("ggplot", "base")) {
  type <- match.arg(type)

  switch (
    type,
    "base" = list(
      value = 1.2,
      min = 0.1,
      max = 10,
      step = 0.1
    ),
    "ggplot" = list(
      value = 12,
      min = 1,
      max = 30,
      step = 1
    ),
    list(
      value = 1.2,
      min = 0.1,
      max = 10,
      step = 0.1
    )
  )
}

defaultInitTitle <- function() {
  defaultInitText()[["plotTitle"]]
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

  if (!all(availableElements %in% configElements)) {
    invalid_elements <- availableElements[!availableElements %in% configElements]
    stop(
      sprintf(
        "Invalid 'availableElements' found: '%s'. 'availableElements' must be one ore more of c('%s')",
        paste0(invalid_elements, collapse = "', '"),
        paste0(configElements, collapse = "', '")
      )
    )
  }

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
    yaxis2 = list(`2nd y axis title` = "yAxisTitle2", `2nd y axis text` = "yAxisText2")
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
    sapply(
      FUN = function(x)
        gsub(
          pattern = ".*\\.",
          replacement = "",
          x = x
        ),
      USE.NAMES = FALSE
    )

  names(x) <- deepestNames
  return(x)
}


# Default Title Format (no docu for 'man' because it is a helper function)
#
# Initial values for title dependent on the plot type
#
# @inheritParams plotTitlesServer
defaultTextFormat <- function(type = c("none", "ggplot", "base")) {
  type <- match.arg(type)

  title <- switch (
    type,
    "none" = config()$defaultBaseTitle,
    "base" = config()$defaultBaseTitle,
    "ggplot" = config()$defaultGGTitle
  )

  text <- switch (
    type,
    "none" = config()$defaultBaseText,
    "base" = config()$defaultBaseText,
    "ggplot" = config()$defaultGGText
  )

  list(title = title, text = text)
}

showTextInputs <- function(element_id) {
  if (length(element_id) == 0) return(FALSE)

  if (element_id %in% c("plotTitle",
                        "xAxisTitle",
                        "yAxisTitle",
                        "yAxisTitle2")) {
    TRUE
  } else {
    FALSE
  }
}

showPositionInputs <- function(element_id) {
  if (length(element_id) == 0) return(FALSE)

  if (element_id %in% c("xAxisText", "yAxisText", "yAxisText2")) {
    TRUE
  } else {
    FALSE
  }
}

# Observe Text Elements Of Label (no docu for 'man' because it is a helper function)
#
# Observe inputs for different text elements (e.g. color, size, ...) of a selected label (e.g.
# plot title, axis text, ...) and store values in the reactiveValues list 'text'.
#
# @param input input object from server function
# @param output output object from server function
# @param session session from server function
# @param plot_text (reactiveValue) contains text elements
# @inheritParams plotTitlesServer
observeAndUpdateTextElements <- function(input,
                                         output,
                                         session,
                                         id,
                                         plot_text,
                                         show_parse_button) {
  # set up all observers for text elements
  # we cannot loop over the elements. When looping reactivity gets lost.

  # keep input useExpression for "updateUserInputs()"
  observe({
    logDebug("%s: Entering observe 'text'", id)

    plot_text[["text"]] <- input[["text"]]
  }) %>%
    bindEvent(input[["text"]])

  observe({
    logDebug("%s: Entering observe 'useExpression'", id)

    plot_text[["useExpression"]] <- input[["useExpression"]]
  }) %>%
    bindEvent(input[["useExpression"]])

  if (show_parse_button) {
    observe({
      logDebug("%s: Entering observe 'parseExpression'", id)

      plot_text[["expression"]] <- input[["expression"]]
    }) %>%
      bindEvent(input[["parseExpression"]])
  } else {
    observe({
      logDebug("%s: Entering observe 'expression'", id)

      plot_text[["expression"]] <- input[["expression"]]
    }) %>%
      bindEvent(input[["expression"]])
  }

  observe({
    logDebug("%s: Entering observe 'fontFamily'", id)

    plot_text[["fontFamily"]] <- input[["fontFamily"]]
  }) %>%
    bindEvent(input[["fontFamily"]])

  observe({
    logDebug("%s: Entering observe 'fontType'", id)

    plot_text[["fontType"]] <- input[["fontType"]]
  }) %>%
    bindEvent(input[["fontType"]])

  observe({
    logDebug("%s: Entering observe 'color'", id)

    plot_text[["color"]] <- input[["color"]]
  }) %>%
    bindEvent(input[["color"]])

  observe({
    logDebug("%s: Entering observe 'size'", id)

    plot_text[["size"]] <- input[["size"]]
  }) %>%
    bindEvent(input[["size"]])

  observe({
    logDebug("%s: Entering observe 'hide'", id)

    plot_text[["hide"]] <- input[["hide"]]
  }) %>%
    bindEvent(input[["hide"]])

  observe({
    logDebug("%s: Entering observe 'angle'", id)

    plot_text[["angle"]] <- input[["angle"]]
  }) %>%
    bindEvent(input[["angle"]])

  observe({
    logDebug("%s: Entering observe 'hjust'", id)

    plot_text[["hjust"]] <- input[["hjust"]]
  }) %>%
    bindEvent(input[["hjust"]])

  observe({
    logDebug("%s: Entering observe 'vjust'", id)

    plot_text[["vjust"]] <- input[["vjust"]]
  }) %>%
    bindEvent(input[["vjust"]])

  return(plot_text)
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
#     formatTextUI(id = "testMod",
#                  type = "ggplot"),
#     tags$h3("Output"),
#     shiny::verbatimTextOutput("titleList")
#   )
# )
#
# server <- function(input, output, session) {
#     thisTitle <- formatTextServer(
#       "testMod",
#       #element_id = reactive("plotTitle"),
#       element_id = reactive("xAxisText"),
#       init_layout = reactive(list(
#         text = "testHeader",
#         fontFamily = "sans",
#         fontType = "italic",
#         color = "#000000",
#         size = 32L,
#         hide = FALSE
#       ))
#     )
#
#     output$titleList <- renderPrint({
#       thisTitle() %>% as.data.frame()
#     })
# }
#
# shinyApp(ui = ui, server = server)
