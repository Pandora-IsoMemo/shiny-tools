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
      choices = c("No label available ..." = ""),
      width = "100%"
    ),

    # extracting UI
    formatTextUI(
      ns("text"),
      type = type,
      initStyle = initText[["plotTitle"]]
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
                                                  text_type = c("title", "axis"),
                                                  show_parse_button = TRUE,
                                                  label_name = reactive(input[["labelName"]]))

                 observe({
                   logDebug("%s: Entering update plotText", id)
                   req(input[["labelName"]])

                   plotText[[input[["labelName"]]]] <- updated_text() #%>%
                     #removeHiddenInputs(names(input))
                 }) %>%
                   bindEvent(updated_text())

                 return(plotText)
               })
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
