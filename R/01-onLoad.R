#' Add path to images
#'
#' @param ... further arguments
#'
#' @return NULL
.onLoad <- function(...) {
  addResourcePath(prefix = "app_files",
                  directoryPath = system.file("dist", package = "shinyTools"))
}
