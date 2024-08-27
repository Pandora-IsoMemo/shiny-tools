#' Plot Legend UI
#'
#' Function to create the UI for the legend
#'
#' @param title (character) title of the module
#' @param titleTag (character) HTML tag to put around the title, e.g. "h4" for \code{h4} from
#' \code{htmltools}
#' @rdname plotLegendServer
plotLegendUI <- function(id, title = NULL, titleTag = "h4") {
  ns <- NS(id)
  tagList(
    setModuleTitle(title = title, titleTag = titleTag),
    selectInput(
      inputId = ns("legendPosition"),
      label = "Legend position",
      choices = c("none", "right", "top", "bottom", "left", "custom")
    ),
    tags$br(),
    conditionalPanel(
      ns = ns,
      condition = "input.legendPosition == 'custom'",
      fluidRow(column(
        6,
        sliderInput(
          inputId = ns("legendX"),
          label = "X position",
          value = 0.9,
          min = 0,
          max = 1
        )
      ), column(
        6,
        sliderInput(
          inputId = ns("legendY"),
          label = "Y position",
          value = 0.1,
          min = 0,
          max = 1
        )
      ))
    )
  )
}

#' Plot Legend Server
#'
#' Function to create the server for the legend
#'
#' @param id namespace id
plotLegendServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    legend <- reactiveValues(position = "none")

    observe({
      if (input[["legendPosition"]] == "custom") {
        legend$position <- c(input[["legendX"]], input[["legendY"]])
      } else {
        legend$position <- input[["legendPosition"]]
      }
    })

    return(legend)
  })
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
#     plotLegendUI(id = "testMod", title = "Legend", titleTag = "h4")
#   )
# )
#
# server <- function(input, output, session) {
#   testPlotFun <- function() {
#     data <- data.frame(
#       x = c(1, 2, 3, 4, 5),
#       y = c(3, 5, 2, 8, 7),
#       group = factor(c("A", "B", "A", "B", "A"))
#     )
#
#     ggplot2::ggplot(data, ggplot2::aes(x = x, y = y, color = group)) +
#       ggplot2::geom_line()
#   }
#
#   thisLegend <- plotLegendServer("testMod")
#
#   output$plot <- renderPlot({
#     testPlotFun() %>%
#       formatLegendOfGGplot(legend = thisLegend)
#   })
# }
#
# shinyApp(ui = ui, server = server)
