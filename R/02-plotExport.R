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
#' @param extraPlotFormatting (character) one of "none", "ggplot". Adds the option to format the
#'  plot before export
#' @param filename (character) name of file without file extension
#' @param plotly (logical) set TRUE if plotFun returns a plotly output
#' @param plotWidth (reactive) default plot width
#' @param plotHeight (reactive) default plot height
#'
#' @export
plotExportServer <- function(id,
                             plotFun,
                             extraPlotFormatting = c("none", "ggplot"),
                             filename = sprintf("%s_plot", gsub("-", "", Sys.Date())),
                             plotly = FALSE,
                             plotWidth = reactive(1280),
                             plotHeight = reactive(800)) {
  extraPlotFormatting <- match.arg(extraPlotFormatting)
  formatFun <- switch (extraPlotFormatting,
                       "none" = noExtraFormat,
                       "ggplot" = formatTitlesOfGGPlot
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
                       column(4, plotTitlesUI(session$ns("titlesFormat"),
                                              extraPlotFormatting = "ggplot"))
                     ),
                     downloadButton(session$ns("exportExecute"), "Export"),
                     easyClose = TRUE
                   ))
                 })

                 titles <- plotTitlesServer("titlesFormat")

                 output$plot <- renderPlot({
                   plotFun()() %>%
                     formatFun(plotTitle = titles[["plot"]],
                                 axisTitleX = titles[["xAxis"]],
                                 axisTitleY = titles[["yAxis"]])
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
                       print(plotFun()() %>% formatFun(plotTitle = titles[["plot"]],
                                                       axisTitleX = titles[["xAxis"]],
                                                       axisTitleY = titles[["yAxis"]]))
                       dev.off()
                     }
                   }
                 )
               })
}

defaultTitleFormat <- function(text = "") {
  list(text = text,
       fontType = "plain",
       color = "#000000",
       size = 12,
       hide = FALSE)
}

noExtraFormat <- function(plot, plotTitle, axisTitleX, axisTitleY) {
  plot
}

formatTitlesOfGGPlot <- function(plot, plotTitle, axisTitleX, axisTitleY) {
  getElementText <- function(title) {
    if (title[["hide"]]) {
      element_blank()
    } else {
      element_text(family = "Arial",
                   size = title[["size"]],
                   face = title[["fontType"]],
                   color = title[["color"]])
    }
  }

  plot +
    labs(title = plotTitle[["text"]], x = axisTitleX[["text"]], y = axisTitleY[["text"]]) +
    theme(
      plot.title =   getElementText(plotTitle),
      axis.title.x = getElementText(axisTitleX),
      axis.title.y = getElementText(axisTitleY)
    )
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
#                    extraPlotFormatting = "ggplot",
#                    filename = "plot")
# }
#
# shinyApp(ui = ui, server = server)
