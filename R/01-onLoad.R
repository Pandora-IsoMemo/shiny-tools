#' Add path to images
#'
#' @param ... further arguments
#'
#' @return NULL
.onLoad <- function(...) {
  addResourcePath(prefix = "shinyTools_files",
                  directoryPath = system.file("dist", package = "shinyTools"))
}
