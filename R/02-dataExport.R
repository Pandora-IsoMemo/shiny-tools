#' Data Export Button
#'
#' @param id module id
#' @param label (character) button label
#'
#' @export
dataExportButton <- function(id, label = "Export Data") {
  ns <- NS(id)
  actionButton(ns("export"), label)
}

#' Server function for data export
#'
#' Backend for data export module
#'
#' @param id namespace id
#' @param dataFun (reactive) a reactive function returning a data.frame for export
#' @param filename (character) name of file without file extension
#'
#' @export
dataExportServer <- function(id, dataFun, filename = "data") {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns

                 observeEvent(input$export, {
                   showModal(modalDialog(
                     "Export Data",
                     easyClose = TRUE,
                     footer = modalButton("OK"),
                     selectInput(
                       ns("exportType"),
                       "File type",
                       choices = c("csv", "xlsx", "json"),
                       selected = "xlsx"
                     ),
                     conditionalPanel(
                       condition = "input['exportType'] == 'csv'",
                       ns = ns,
                       div(style = "display: inline-block;horizontal-align:top; width: 80px;",
                           textInput(ns("colseparator"), "column separator:", value = ",")),
                       div(style = "display: inline-block;horizontal-align:top; width: 80px;",
                           textInput(ns("decseparator"), "decimal separator:", value = "."))
                     ),
                     tags$br(),
                     downloadButton(ns("exportExecute"), "Export")
                   ))
                 })

                 observe({
                   if (length(dataFun()()) == 0)
                     shinyjs::disable(ns("export"), asis = TRUE) else
                       shinyjs::enable(ns("export"), asis = TRUE)
                 })

                 output$exportExecute <- downloadHandler(
                   filename = function(){
                     exportFilename(filename, input$exportType)
                   },
                   content = function(file){
                     switch(
                       input$exportType,
                       csv = exportCSV(file, dataFun()(), input$colseparator, input$decseparator),
                       xlsx = exportXLSX(file, dataFun()()),
                       json = exportJSON(file, dataFun()())
                     )
                   }
                 )
               })
}

#' Filename of Export
#'
#' @param fileending character csv or xlsx
#' @param filename name of file
exportFilename <- function(filename, fileending){
  paste(filename, fileending, sep = ".")
}

#' Export to csv
#'
#' @param file filename
#' @param dat data.frame
#' @param colseparator column seperator
#' @param decseparator decimal seperator
exportCSV <- function(file, dat, colseparator, decseparator){
  write.table(x = dat, file = file, sep = colseparator,
              dec = decseparator, row.names = FALSE)
}

#' Export to xlsx
#'
#' @param file filename
#' @param dat data.frame
exportXLSX <- function(file, dat){
  write.xlsx(dat, file)
}

#' Export to json
#'
#' @param file filename
#' @param dat data.frame
exportJSON <- function(file, dat){
  json <- toJSON(dat)
  write(json, file)
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
#     dataExportButton(id = "expData")
#   )
# )
#
# server <- function(input, output, session) {
#   dataExportServer("expData", dataFun = reactive({function() mtcars}), filename = "data")
# }
#
# shinyApp(ui = ui, server = server)
