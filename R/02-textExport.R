#' Text Export Button
#'
#' @param label (character) button label
#' @rdname textExportServer
#'
#' @export
textExportButton <- function(id, label = "Export") {
  ns <- NS(id)
  downloadButton(ns("download"), "Export", icon = NULL)
}


#' Server function for text export
#'
#' Backend for export module
#'
#' @param id namespace id
#' @param outFun (reactive) a reactive function returning the output for export, e.g. print, summary, ...
#' @param filename (character) name of file without file extension
#'
#' @export
textExportServer <- function(id, outFun, filename) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns

                 # for debugging
                 # observe({
                 #   req(outFun()())
                 #   browser()
                 #   outFun()() %>% capture.output()
                 # })

                 observe({
                   logDebug("%s: Entering enable/disable button 'download'", id)

                   res <- try(outFun()())
                   if (inherits(res, "try-error") || length(res) == 0)
                     shinyjs::disable(ns("download"), asis = TRUE) else
                       shinyjs::enable(ns("download"), asis = TRUE)
                 })

                 output[["download"]] <- downloadHandler(
                   filename = function() {
                     paste0(resolveValue(filename), ".txt")
                   },
                   content = function(file) {
                     res <- outFun()() %>%
                       capture.output()

                     writeLines(res, file)
                   }
                 )
               })
}
