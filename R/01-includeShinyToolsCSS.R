#' Include CSS from shinyTools package
#'
#' @return NULL
#' @export
includeShinyToolsCSS <- function(){
  tags$link(href = "shinyTools_files/custom.css", rel = "stylesheet")
}
