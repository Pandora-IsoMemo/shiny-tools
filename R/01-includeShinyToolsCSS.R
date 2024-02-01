#' Include CSS from shinyTools package
#'
#' @return NULL
#' @export
includeShinyToolsCSS <- function(){
  tags$link(href="app_files/custom.css", rel="stylesheet")
}
