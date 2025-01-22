#' Format Line UI
#'
#' @inheritParams setModuleTitle
#' @rdname formatLineServer
#'
#' @export
formatLineUI <- function(id,
                         title = NULL,
                         titleTag = "h4",
                         initStyle = defaultLineFormat()) {
  ns <- NS(id)
  tagList(
    setModuleTitle(title = title, titleTag = titleTag),
    checkboxInput(
      inputId = ns("hide"),
      label = "Hide Line",
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
      min = 0.1,
      max = 10,
      width = "100%"
    ),
    sliderInput(
      inputId = ns("horizontalcaps"),
      label = "Cap Size (horizontal)",
      min = 0.1,
      max = 2,
      value = initStyle[["horizontalcaps"]],
      width = "100%"
    ),
    sliderInput(
      inputId = ns("verticalcaps"),
      label = "Cap Size (vertical)",
      min = 0.1,
      max = 2,
      value = initStyle[["verticalcaps"]],
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
    )
  )
}

#' Server function for format line
#'
#' Backend for format line module
#'
#' @param id namespace id
#' @param hideInput (character) inputs that should be disabled (hidden) when applying this module.
#'  Possible inputs are "hide", "symbol", "color", "size", "alpha", "colorBg", "lineWidthBg".
#'  Please use \code{shinyjs::useShinyjs()} in your UI function to enable this feature.
#' @param initStyle (list) optional, named list with style definitions, should have the same format
#'  as the default output of \code{plotPointsServer}
#' @param reloadInit (reactiveVal) logical, should \code{initStyle} be reloaded?
#'
#' @export
formatLineServer <- function(id,
                             hideInput = c(),
                             initStyle = defaultLineFormat(),
                             reloadInit = reactiveVal(FALSE)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    style <- custom_style <- initializeReactiveObject(
      session,
      id,
      custom_values = initStyle,
      choices = c(
        "size",
        "linetype",
        "horizontalcaps",
        "verticalcaps",
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
      req(isTRUE(reloadInit()))
      logDebug("%s: Entering reload 'initStyle'", id)

      if (is.reactive(initStyle)) {
        new_inputs <- initStyle()
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
      bindEvent(reloadInit())

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
    logDebug("%s: Entering observe 'horizontalcaps'", id)

    style[["horizontalcaps"]] <- input[["horizontalcaps"]]
  }) %>%
    bindEvent(input[["horizontalcaps"]])

  observe({
    logDebug("%s: Entering observe 'verticalcaps'", id)

    style[["verticalcaps"]] <- input[["verticalcaps"]]
  }) %>%
    bindEvent(input[["verticalcaps"]])

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
#   list(linetype = 2, color = "#00FF22", alpha = 0.3, capsize = 1, hide = FALSE)
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
#   thisStyle <- formatLineServer(id = "testMod", initStyle = testStyle())
#
#   output$titleList <- renderPrint({
#     thisStyle %>% reactiveValuesToList() %>% as.data.frame()
#   })
# }
#
# shinyApp(ui = ui, server = server)
