# Text Format UI
#
# @param id id of parent(!) module
# @rdname plotTitlesServer
#
# @return tagList
formatTextUI <- function(id,
                         type = c("ggplot", "base", "none"),
                         initTitle = NULL,
                         initAxis = NULL) {
  type <- match.arg(type)

  if (is.null(initTitle)) {
    # if null: take values from config
    initTitle <- defaultTextFormat(type = type)[["title"]]
  }

  if (is.null(initAxis)) {
    # if null: take values from config
    initAxis <- defaultTextFormat(type = type)[["text"]]
  }

  ns <- NS(id)
  tagList(
    checkboxInput(
      inputId = ns("hide"),
      label = "Hide Text",
      value = initTitle[["hide"]],
      width = "100%"
    ),
    conditionalPanel(
      ns = ns,
      condition = "['legendTitle', 'plotTitle', 'xAxisTitle', 'yAxisTitle', 'yAxisTitle2'].includes(output.label_name)",
      checkboxInput(
        ns("useExpression"),
        label = "Use mathematical annotation",
        value = FALSE,
        width = "100%"
      ),
      conditionalPanel(
        ns = ns,
        condition = "input.useExpression",
        uiOutput(ns("expressionInput")),
        helpText(
          'Example: Use \u003C"Bayesian Estimated" ~ delta^~13~C ~ ("\u2030" - ~ "VPDB")\u003E for \u003C"Bayesian Estimated \u03B4\u00B9\u00B3C (\u2030 - VPDB)"\u003E.',
          width = "100%"
        ),
        helpText(
          HTML(
            'Note: "Font type" input is not available for "Expression". For more information, visit the <a href="https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/plotmath.html" target="_blank">R Documentation</a>.'
          ),
          width = "100%"
        )
      ),
      conditionalPanel(
        ns = ns,
        condition = "!input.useExpression",
        textInput(
          ns("text"),
          label = "Text",
          value = initTitle[["text"]],
          placeholder = "Custom title ..."
        )
      ),
    ),
    conditionalPanel(
      ns = ns,
      condition = "!input.useExpression",
      selectInput(
        ns("fontType"),
        label = "Font type",
        choices = fontChoicesSelect(type = type),
        selected = initTitle[["fontType"]],
        width = "100%"
      )
    ),
    colourInput(
      ns("color"),
      label = "Text color",
      value = initTitle[["color"]],
      width = "100%"
    ),
    selectInput(
      inputId = ns("fontFamily"),
      label = "Font family",
      selected = initTitle[["fontFamily"]],
      choices = availableFonts(),
      width = "100%"
    ),
    sliderInput(
      ns("size"),
      label = "Text size",
      value = initTitle[["size"]],
      min = sizeValuesSlider(type = type)[["min"]],
      max = sizeValuesSlider(type = type)[["max"]],
      step = sizeValuesSlider(type = type)[["step"]],
      width = "100%"
    ),
    conditionalPanel(
      ns = ns,
      condition = "['xAxisText', 'yAxisText', 'yAxisText2'].includes(output.label_name)",
      sliderInput(
        ns("angle"),
        label = "Text angle",
        value = initAxis[["angle"]],
        min = 0,
        max = 360,
        step = 5
      ),
      sliderInput(
        ns("hjust"),
        label = "Horizontal adjustment",
        value = initAxis[["hjust"]],
        min = 0,
        max = 1,
        step = 0.1
      ),
      sliderInput(
        ns("vjust"),
        label = "Vertical adjustment",
        value = initAxis[["vjust"]],
        min = 0,
        max = 1,
        step = 0.1
      )
    )
  )
}

# Server function for plot titles
#
# Backend for plot titles module
#
# @param id namespace id
# @param plot_type (character) Type of the plot to add titles to, one of "none", "ggplot", "base".
# @param text_type (character) Type of formatting, e.g. for titles or for axis
# @param availableElements (character) set of available labels for specifying the format of text.
#  May contain elements from \code{c("title", "axis", "yaxis2", "legend")}.
# @param show_parse_button (logical) Show parse button for parsing mathematical expressions.
# @inheritParams plotExportServer
formatTextServer <- function(id,
                             init_text = reactive(defaultInitTitle()),
                             plot_type = c("none", "ggplot", "base"),
                             text_type = c("title", "axis"),
                             show_parse_button = TRUE,
                             label_name = reactive("plotTitle")) {
  plot_type <- match.arg(plot_type)

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    new_text <- reactiveValues()

    observe({
      logDebug("%s: Entering observe 'init_text'", id)
      for (name in names(init_text())) {
        new_text[[name]] <- init_text()[[name]]
      }
    }) %>%
      bindEvent(init_text())

    # Bind the reactive value to an output for the conditionalPanel
    output$label_name <- reactive({
      label_name()
    })

    # Mark the output as usable in conditionalPanel
    outputOptions(output, "label_name", suspendWhenHidden = FALSE)

    output$expressionInput <- renderUI({
      if (show_parse_button) {
        fluidRow(
          column(
            width = 8,
            textInput(
              ns("expression"),
              label = "Expression",
              value = init_text()[["text"]],
              placeholder = "Custom title ..."
            )
          ),
          column(
            width = 4,
            style = "margin-top: 1.7em;",
            actionButton(ns("parseExpression"), "Parse", width = "100%")
          )
        )
      } else {
        textInput(
          ns("expression"),
          label = "Expression",
          value = init_text()[["expression"]],
          placeholder = "Custom title ..."
        )
      }
    })

    observe({
      req(label_name())
      logDebug("%s: Entering observe 'label_name'", id)

      updateUserInputs(
        input = input,
        output = output,
        session = session,
        userInputs = init_text()
      )
    }) %>%
      bindEvent(label_name())

    new_text <- observeAndUpdateTextElements(input, output, session, id, new_text, show_parse_button)

    return(reactive(reactiveValuesToList(new_text)))
  })
}

defaultInitTitle <- function() {
  defaultInitText()[["plotTitle"]]
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
#       plot_type = "ggplot",
#       text_type = "title",
#       #label_name = reactive("plotTitle"),
#       label_name = reactive("xAxisText"),
#       initText = list(
#         text = "testHeader",
#         fontFamily = "sans",
#         fontType = "italic",
#         color = "#000000",
#         size = 32L,
#         hide = FALSE
#       )
#     )
#
#     output$titleList <- renderPrint({
#       thisTitle %>%
#         reactiveValuesToList() %>% dput()
#     })
# }
#
# shinyApp(ui = ui, server = server)
