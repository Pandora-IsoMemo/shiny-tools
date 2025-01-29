# Format Line UI
#
# @inheritParams setModuleTitle
# @rdname formatLineServer
formatLineUI <- function(id,
                         title = NULL,
                         titleTag = "h4",
                         initStyle = defaultLineFormat()) {
  ns <- NS(id)
  tagList(
    setModuleTitle(title = title, titleTag = titleTag),
    checkboxInput(
      inputId = ns("hide"),
      label = "Hide",
      value = initStyle[["hide"]],
      width = "100%"
    ),
    selectInput(
      inputId = ns("linetype"),
      label = "linetype",
      choices = c(
        "solid" = "1",
        "dashed" = "2",
        "dotted" = "3",
        "dotdash" = "4",
        "longdash" = "5",
        "twodash" = "6"
      ),
      selected = initStyle[["linetype"]],
      width = "100%"
    ),
    sliderInput(
      ns("size"),
      label = "Thickness",
      value = initStyle[["size"]],
      min = 0,
      max = 10,
      step = 0.1,
      width = "100%"
    ),
    colourInput(
      ns("color"),
      label = "Line color",
      value = initStyle[["color"]],
      width = "100%"
    ),
    sliderInput(
      inputId = ns("alpha"),
      label = "Opacity",
      min = 0,
      max = 1,
      value = initStyle[["alpha"]],
      width = "100%"
    ),
    # next inputs are only relevant for error bars and set the size of the caps (whiskers)
    # if this module will be used for other lines, we can add a condition here
    numericInput(
      inputId = ns("capheight"),
      label = "X Error Cap Height",
      min = 0,
      value = initStyle[["capheight"]],
      width = "100%"
    ),
    numericInput(
      inputId = ns("capwidth"),
      label = "Y Error Cap Width",
      min = 0,
      value = initStyle[["capwidth"]],
      width = "100%"
    )
  )
}

# Server function for format line
#
# Backend for format line module
#
# @param id namespace id
# @param hideInput (character) inputs that should be disabled (hidden) when applying this module.
#  Possible inputs are "hide", "symbol", "color", "size", "alpha", "colorBg", "lineWidthBg".
#  Please use \code{shinyjs::useShinyjs()} in your UI function to enable this feature.
# @param init_layout (list) optional, named list with style definitions, should have the same format
#  as the default output of \code{plotPointsServer}
# @param element_id (reactiveVal) logical, should \code{init_layout} be reloaded?
formatLineServer <- function(id,
                             hideInput = c(),
                             init_layout = defaultLineFormat(),
                             element_id = reactiveVal(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    style <- custom_style <- initializeReactiveObject(
      session,
      id,
      custom_values = init_layout,
      choices = c(
        "size",
        "linetype",
        "capwidth",
        "capheight",
        "color",
        "alpha",
        "hide"
      ),
      default_fun = defaultLineFormat
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
      req(element_id())
      logDebug("%s: Entering reload 'init_layout'", id)

      if (is.reactive(init_layout)) {
        new_inputs <- init_layout()
      } else {
        new_inputs <- custom_style
      }

      updateUserInputs(
        input = input,
        output = output,
        session = session,
        userInputs = new_inputs
      )
    }) %>%
      bindEvent(element_id())

    style <- observeAndUpdateLineElements(input, output, session, id, style = style)

    return(style)
  })
}

observeAndUpdateLineElements <- function(input, output, session, id, style) {
  observe({
    logDebug("%s: Entering observe 'linetype'", id)

    style[["linetype"]] <- as.numeric(input[["linetype"]])
  }) %>%
    bindEvent(input[["linetype"]])

  observe({
    logDebug("%s: Entering observe 'color'", id)

    style[["color"]] <- input[["color"]]
  }) %>%
    bindEvent(input[["color"]])

  observe({
    logDebug("%s: Entering observe 'alpha'", id)

    style[["alpha"]] <- input[["alpha"]]
  }) %>%
    bindEvent(input[["alpha"]])

  observe({
    logDebug("%s: Entering observe 'size'", id)

    style[["size"]] <- input[["size"]]
  }) %>%
    bindEvent(input[["size"]])

  observe({
    logDebug("%s: Entering observe 'capwidth'", id)

    style[["capwidth"]] <- input[["capwidth"]]
  }) %>%
    bindEvent(input[["capwidth"]])

  observe({
    logDebug("%s: Entering observe 'capheight'", id)

    style[["capheight"]] <- input[["capheight"]]
  }) %>%
    bindEvent(input[["capheight"]])

  observe({
    logDebug("%s: Entering observe 'hide'", id)

    style[["hide"]] <- input[["hide"]]
  }) %>%
    bindEvent(input[["hide"]])

  return(style)
}

defaultLineFormat <- function() {
  config()$defaultLineStyle
}

# TEST MODULE -------------------------------------------------------------
# To test the module run devtools::load_all() first
# Please comment this code before building the package

# testStyle <- function() {
#   list(linetype = 2, color = "#00FF22", size = 1, alpha = 0.3, capwidth = 1, capheight = 0, hide = FALSE)
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
#     shiny::verbatimTextOutput("titleList"),
#     formatLineUI(id = "testMod", initStyle = testStyle())
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
#   thisStyle <- formatLineServer(id = "testMod", init_layout = testStyle())
#
#   output$titleList <- renderPrint({
#     thisStyle %>% reactiveValuesToList() %>% as.data.frame()
#   })
# }
#
# shinyApp(ui = ui, server = server)
