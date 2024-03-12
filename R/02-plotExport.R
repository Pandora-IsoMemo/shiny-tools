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
#' @param plotType (character) one of "none", "ggplot". Adds the option to format the
#'  plot before export
#' @param filename (character) name of file without file extension
#' @param plotly (logical) set TRUE if plotFun returns a plotly output
#' @param plotWidth (reactive) default plot width
#' @param plotHeight (reactive) default plot height
#' @param titles (list) optional, named list with title definitions, output of \code{plotTitlesServer}
#' @param ranges (list) optional, named list with range definitions, output of \code{plotRangesServer}
#'
#' @export
plotExportServer <- function(id,
                             plotFun,
                             plotType = c("none", "ggplot"),
                             filename = sprintf("%s_plot", gsub("-", "", Sys.Date())),
                             plotly = FALSE,
                             plotWidth = reactive(1280),
                             plotHeight = reactive(800),
                             titles = NULL,
                             ranges = NULL) {
  plotType <- match.arg(plotType)
  formatFun <- switch (plotType,
                       "none" = noExtraFormat,
                       "ggplot" = formatWrapperGGplot
  )

  moduleServer(id,
               function(input, output, session) {
                 observeEvent(input$export, {
                   plotOutputElement <- if (plotly) {
                     plotlyOutput(session$ns("plotly"))
                   } else {
                     plotOutput(session$ns("plot"), height = "300px")
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
                                session$ns("exportType"), "Filetype",
                                choices = exportTypeChoices
                              ),
                              if (!plotly) numericInput(session$ns("width"), "Width (px)", value = plotWidth()) else NULL,
                              if (!plotly) numericInput(session$ns("height"), "Height (px)", value = plotHeight()) else NULL,
                       ),
                       if (plotType == "ggplot") {
                         column(4, plotTitlesUI(session$ns("titlesFormat"), type = "ggplot"))
                       } else NULL,
                       if (plotType == "ggplot") {
                         column(4, plotRangesUI(session$ns("axesRanges"), type = "ggplot"))
                       }
                     ),
                     tags$br(),
                     downloadButton(session$ns("exportExecute"), "Export"),
                     easyClose = TRUE
                   ))
                 })

                 titles <- plotTitlesServer("titlesFormat", type = plotType, titles = titles)
                 ranges <- plotRangesServer("axesRanges", type = plotType, ranges = ranges)

                 output$plot <- renderPlot({
                   plotFun()() %>% formatFun(titles = titles, ranges = ranges)
                 })

                 output$plotly <- renderPlotly({
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
                       print(plotFun()() %>% formatFun(titles = titles, ranges = ranges))
                       dev.off()
                     }
                   }
                 )
               })
}

noExtraFormat <- function(plot, ...) {
  plot
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
#     plotExportButton(id = "expPlot")
#   )
# )
#
# server <- function(input, output, session) {
#   plotExportServer("expPlot",
#                    plotFun = reactive({
#                      function() {
#                        data <- data.frame(
#                          x = c(1, 2, 3, 4, 5),
#                          y = c(2, 4, 1, 7, 3)
#                        )
#
#                        ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) +
#                          ggplot2::geom_point()
#                      }
#                      }),
#                    plotType = "ggplot",
#                    filename = "plot",
#                    titles = list(plot = list(text = "testHeader", fontType = "italic", color = "#000000",
#                                              size = 32L, hide = FALSE),
#                                  xAxis = list(text = "test", fontType = "bold",
#                                               color = "#FFFFFF", size = 25, hide = FALSE),
#                                  yAxis = list(text = "", fontType = "plain", color = "#000000",
#                                               size = 12L, hide = FALSE)),
#                    ranges = list(xAxis = list(min = NA, max = 20),
#                                  yAxis = list(min = 0L, max = 10L)))
# }
#
# shinyApp(ui = ui, server = server)
