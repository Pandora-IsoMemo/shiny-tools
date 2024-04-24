#' Plot Export Button
#'
#' @param id module id
#' @param label (character) button label
#'
#' @return actionButton
#' @export
plotExportButton <- function(id, label = "Export Plot") {
  ns <- NS(id)
  actionButton(ns("export"), label = label)
}

#' Server function for plot export
#'
#' Backend for plot export module
#'
#' @param id namespace id
#' @param plotFun (reactive) a reactive function returning a plot for export
#' @param plotType (character) one of "none", "ggplot", "ggplot_only_titles". Adds the option to
#'  format titles and ranges of a plot within the export UI (currently only for ggplots). For
#'  \code{plotType == "ggplot_only_titles"} only titles can be adjusted. This prevents that custom
#'  formatting of axis ranges might be overwritten by \code{formatRangesOfGGplot()}.
#' @param filename (character) name of file without file extension
#' @param plotly (logical) set TRUE if plotFun returns a plotly output
#' @param plotWidth (reactive) default plot width
#' @param plotHeight (reactive) default plot height
#' @param initText (list) optional, named list with title definitions, or output of \code{plotTitlesServer}
#' @param initRanges (list) optional, named list with range definitions, or output of \code{plotRangesServer}
#'
#' @export
plotExportServer <- function(id,
                             plotFun,
                             plotType = c("none", "ggplot", "ggplot_only_titles"),
                             filename = sprintf("%s_plot", gsub("-", "", Sys.Date())),
                             plotly = FALSE,
                             plotWidth = reactive(1280),
                             plotHeight = reactive(800),
                             initText = NULL,
                             initRanges = NULL) {
  plotType <- match.arg(plotType)
  formatFun <- switch (plotType,
                       "none" = noExtraFormat,
                       "ggplot" = formatWrapperGGplot,
                       "ggplot_only_titles" = function(plot, text, ranges, what = c("titles"))
                         formatWrapperGGplot(plot, text, ranges, what)
  )

  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns

                 observe({
                   if (inherits(initText, "reactivevalues"))
                     initText <- reactiveValuesToList(initText)

                   plotOutputElement <- if (plotly) {
                     plotlyOutput(ns("exportPlotly"))
                   } else {
                     plotOutput(ns("exportPlot"), height = "300px")
                   }

                   exportTypeChoices <- if (plotly) {
                     c("png", "jpeg", "svg", "pdf")
                   } else {
                     c("png", "pdf", "svg", "tiff")
                   }

                   showModal(modalDialog(
                     title = "Export Graphic",
                     footer = modalButton("OK"),
                     plotOutputElement,
                     fluidRow(
                       column(4,
                              h4("File"),
                              selectInput(
                                ns("exportType"), "Filetype",
                                choices = exportTypeChoices
                              ),
                              if (!plotly) numericInput(ns("width"), "Width (px)", value = plotWidth()) else NULL,
                              if (!plotly) numericInput(ns("height"), "Height (px)", value = plotHeight()) else NULL,
                       ),
                       if (plotType %in% c("ggplot", "ggplot_only_titles")) {
                         column(4, plotTitlesUI(ns("titlesFormat"),
                                                type = "ggplot",
                                                initText = initText))
                       } else NULL,
                       if (plotType == "ggplot") {
                         column(4, plotRangesUI(ns("axesRanges"),
                                                initRanges = initRanges))
                       }
                     ),
                     tags$br(),
                     downloadButton(ns("exportExecute"), "Export"),
                     easyClose = TRUE
                   ))
                 }) %>%
                   bindEvent(input$export)

                 observe({
                   if (length(plotFun()()) == 0)
                     shinyjs::disable(ns("export"), asis = TRUE) else
                       shinyjs::enable(ns("export"), asis = TRUE)
                 })

                 text <- plotTitlesServer("titlesFormat",
                                          type = extractType(plotType),
                                          initText = initText)
                 ranges <- plotRangesServer("axesRanges",
                                            type = extractType(plotType),
                                            initRanges = initRanges)

                 output$exportPlot <- renderPlot({
                   plotFun()() %>%
                     formatFun(text = text, ranges = ranges)
                 })

                 output$exportPlotly <- renderPlotly({
                   plotFun()()
                 })

                 output$exportExecute <- downloadHandler(
                   filename = function() {
                     paste0(filename, ".", input$exportType)
                   },
                   content = function(file) {
                     if (plotly) {
                       tmpfile <- paste0("plot.", input$exportType)
                       save_image(plotFun()(), file = tmpfile) %>%
                         tryCatchWithWarningsAndErrors(errorTitle = "Export failed", alertStyle = "shinyalert")
                       file.copy(tmpfile, file)
                     } else {
                       switch(input$exportType,
                              png = png(file, width = input$width, height = input$height),
                              pdf = pdf(file, width = input$width / 72, height = input$height / 72),
                              tiff = tiff(file, width = input$width, height = input$height),
                              svg = svg(file, width = input$width / 72, height = input$height / 72)
                       )
                       print(
                         plotFun()() %>%
                           formatFun(text = text, ranges = ranges)
                       )
                       dev.off()
                     }
                   }
                 )
               })
}

noExtraFormat <- function(plot, ...) {
  plot
}

formatWrapperGGplot <- function(plot, text, ranges, what = c("titles", "ranges")) {
  if ("titles" %in% what) {
    plot <- plot %>%
      formatTitlesOfGGplot(text = text)
  }

  if ("ranges" %in% what) {
    plot <- plot %>%
      formatRangesOfGGplot(ranges = ranges)
  }

  plot
}

#' Extract Type
#'
#' @inheritParams plotExportServer
#'
#' @return (character) type required for \code{plotTitlesServer} or \code{plotRangesServer}
extractType <- function(plotType) {
  unlist(strsplit(plotType, split = "_"))[1]
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
#     plotExportButton(id = "expPlot")
#   )
# )
#
# server <- function(input, output, session) {
#   testTitles <- reactiveValues(
#     plotTitle = list(text = "testHeader", fontType = "italic", color = "#000000",
#                 size = 30, hide = FALSE),
#     xAxisTitle = list(text = "test", fontType = "bold",
#                  color = "#FF00EA", size = 25, hide = FALSE),
#     yAxisTitle = list(text = "", fontType = "plain", color = "#000000",
#                  size = 12L, hide = FALSE),
#     xAxisText = list(fontType = "bold",
#                       color = "#FF00EA", size = 25, hide = FALSE),
#     yAxisText = list(fontType = "plain", color = "#000000",
#                       size = 12L, hide = FALSE)
#   )
#
#   testRanges <- reactiveValues(
#     xAxis = list(min = 0L, max = 20, fromData = TRUE),
#     yAxis = list(min = 0L, max = 10L, fromData = FALSE)
#   )
#
#   testPlotFun <- function() {
#     data <- data.frame(
#       x = c(1, 2, 3, 4, 5),
#       y = c(2, 4, 1, 7, 3)
#     )
#
#     ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) +
#       ggplot2::geom_point()
#   }
#
#   output$plot <- renderPlot({
#     testPlotFun() %>%
#       formatTitlesOfGGplot(text = testTitles) %>%
#       formatRangesOfGGplot(ranges = testRanges)
#   })
#
#   plotExportServer("expPlot",
#                    plotFun = reactive({ testPlotFun }),
#                    plotType = "ggplot",
#                    filename = "plot",
#                    initText = testTitles,
#                    initRanges = testRanges)
# }
#
# shinyApp(ui = ui, server = server)
