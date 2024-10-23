#' Resolve Value
#'
#' Function to resolve the value, whether reactive or not
#'
#' @param var The variable to resolve, can be reactive or not
#'
#' @return The resolved value
#'
#' @export
resolveValue <- function(var) {
  if (is.reactive(var)) {
    return(var())  # Call the reactive to get its value
  } else {
    return(var)  # Return the value directly if it's not reactive
  }
}
