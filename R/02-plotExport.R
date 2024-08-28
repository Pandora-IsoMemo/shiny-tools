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
  if (plotly == TRUE && plotType != "none") {
    # currently we have not defined any formatting functions for plotly plots
    warning("Formatting of plotly plots is not supported yet. Setting plotType to 'none'.")
    plotType <- "none"
  }
  formatFun <- switch(plotType,
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
                   res <- try(plotFun()())
                   if (inherits(res, "try-error") || length(res) == 0)
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
                     formatFun(text = text, ranges = ranges) %>%
                     print() %>%
                     shinyTryCatch(errorTitle = "Plot failed",
                                   alertStyle = "shinyalert",
                                   inShiny = FALSE)
                 })

                 output$exportPlotly <- renderPlotly({
                   plotFun()() %>%
                     print() %>%
                     shinyTryCatch(errorTitle = "Plot failed",
                                   alertStyle = "shinyalert",
                                   inShiny = FALSE)
                 })

                 output$exportExecute <- downloadHandler(
                   filename = function() {
                     paste0(filename, ".", input$exportType)
                   },
                   content = function(file) {
                     if (plotly) {
                       tmpfile <- paste0("plot.", input$exportType)
                       p <- plotFun()() %>%
                         print() %>%
                         shinyTryCatch(errorTitle = "Plot failed",
                                       alertStyle = "shinyalert",
                                       inShiny = FALSE)
                       save_image(p, file = tmpfile)
                       file.copy(tmpfile, file)
                     } else {
                       switch(input$exportType,
                              png = png(file, width = input$width, height = input$height),
                              pdf = pdf(file, width = input$width / 72, height = input$height / 72),
                              tiff = tiff(file, width = input$width, height = input$height),
                              svg = svg(file, width = input$width / 72, height = input$height / 72)
                       )
                       plotFun()() %>%
                         formatFun(text = text, ranges = ranges) %>%
                         print() %>%
                         shinyTryCatch(errorTitle = "Plot failed",
                                       alertStyle = "shinyalert",
                                       inShiny = FALSE)
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
#     plotlyOutput("plotly"),
#     plotExportButton(id = "expPlot", label = "Export GGplot"),
#     plotExportButton(id = "expPlotly", label = "Export plotly")
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
#                       color = "#FF00EA", size = 25, hide = FALSE, angle = 45, hjust = 1, vjust = 0.5),
#     yAxisText = list(fontType = "plain", color = "#000000",
#                       size = 12L, hide = FALSE, angle = 0, hjust = 0.5, vjust = 0.5),
#     legendTitle = list(text = "", fontType = "plain", color = "#000000",
#                        size = 12L, hide = FALSE),
#     legendText = list(fontType = "plain",
#                       color = "#FF00EA", size = 25, hide = FALSE, angle = 45, hjust = 1),
#   )
#
#   testRanges <- reactiveValues(
#     xAxis = list(min = 0L, max = 20, fromData = TRUE),
#     yAxis = list(min = 0L, max = 10L, fromData = FALSE)
#   )
#
#   testPlotFun <- function() {
#     p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = factor(cyl), y = mpg)) +
#       ggplot2::geom_boxplot() +
#       ggplot2::labs(title = "Boxplot of MPG by Cylinder",
#                     x = "Number of Cylinders",
#                     y = "Miles per Gallon")
#
#     # in order to force an error use:
#     #p <- p + ggplot2::xlim(c(3, 8))
#   }
#
#   output$plot <- renderPlot({
#     testPlotFun() %>%
#       formatTitlesOfGGplot(text = testTitles) %>%
#       formatRangesOfGGplot(ranges = testRanges) %>%
#       print() %>%
#       shinyTryCatch(errorTitle = "Plot failed", alertStyle = "shinyalert")
#   })
#
#   plotExportServer("expPlot",
#                    plotFun = reactive({ testPlotFun }),
#                    plotType = "ggplot",
#                    filename = "plot",
#                    initText = testTitles,
#                    initRanges = testRanges)
#
#   testPlotlyFun <- function() {
#     p <- plotly::plot_ly(mtcars, x = ~factor(cyl),
#                          y = ~mpg,
#                          # in order to force an error use:
#                          #y = ~non_existent_column,
#                          type = "box") %>%
#       plotly::layout(title = "Boxplot of MPG by Cylinder",
#                      xaxis = list(title = "Number of Cylinders", range = c(0, 10)),
#                      yaxis = list(title = "Miles per Gallon"))
#     p
#   }
#
#   output$plotly <- renderPlotly({
#     testPlotlyFun() %>%
#       print() %>%
#       shinyTryCatch(errorTitle = "Plot failed", alertStyle = "shinyalert")
#   })
#
#   plotExportServer("expPlotly",
#                    plotFun = reactive({ testPlotlyFun }),
#                    plotType = "ggplot",
#                    plotly = TRUE,
#                    filename = "plot",
#                    initText = testTitles,
#                    initRanges = testRanges)
# }
#
# shinyApp(ui = ui, server = server)
