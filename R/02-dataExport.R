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
  if (length(dat) == 0 || !is.list(dat) || (is.list(dat) && any(!sapply(dat, is.data.frame))))
    stop("Wrong type of 'dat'! 'dat' is neither a data.frame nor a list of data.frames.")

  write.table(x = dat, file = file, sep = colseparator,
              dec = decseparator, row.names = FALSE)
}

#' Export to xlsx
#'
#' Export a single data.frame. Or, export a list of data.frames into workbook containing one
#' data.frame in each sheet of a workbook. Sheet names are taken from the names of the list.
#'
#' @param file filename
#' @param dat (list) If \code{dat} is a data.frame \code{dat} will be written into a single sheet
#'  "Sheet 1". If \code{dat} is a named list of data.frames \code{dat} each data.frame is written
#'   into a separate sheet with sheet names taken from the names of the list.
#'
#' @export
exportXLSX <- function(file, dat){
  # Export a single data.frame
  if (is.data.frame(dat)) {
    write.xlsx(dat, file)
    return()
  }

  # abort if wrong type of dat
  if (length(dat) == 0 || !is.list(dat) || (is.list(dat) && any(!sapply(dat, is.data.frame))))
    stop("Wrong type of 'dat'! 'dat' is neither a data.frame nor a list of data.frames.")

  # Or export a workbook containing a data.frame from a list in each sheet
  wb <- createWorkbook()

  sheetName <- names(dat)

  # Loop through each dataframe in the list and write it to a separate sheet in the Excel file
  for (i in seq_along(dat)) {
    # Add a new worksheet with a specific name
    addWorksheet(wb, sheetName = names(dat)[i])
    # Write the dataframe to the newly added worksheet
    writeData(wb, sheet = i, x = dat[[i]])
  }

  # Save the Excel workbook
  saveWorkbook(wb, file = file, overwrite = TRUE)
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
