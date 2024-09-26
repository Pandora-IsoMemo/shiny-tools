#' Convert to Expression
#'
#' Convert a string to an expression
#'
#' @param string (character) string to convert
#'
#' @return expression
convertToExpression <- function(string) {
  if (is.null(string)) {
    return("")
    # ggplot fails for empty expressions like: expression()
  }

  # Trim any leading or trailing whitespace
  trimmed_string <- trimws(string)

  # Return an empty string if the input is empty
  if (nchar(trimmed_string) == 0) {
    return("")
    # ggplot fails for empty expressions like: expression()
  }

  # Convert the string to an expression
  expression_string <- paste0("expression(", trimmed_string, ")")
  # parse the expression
  tryCatch({
    parsed_expression <- eval(parse(text = expression_string))
  }, error = function(e) {
    stop(sprintf("Error in parsing expression: %s", e))
  })

  return(parsed_expression)
}
