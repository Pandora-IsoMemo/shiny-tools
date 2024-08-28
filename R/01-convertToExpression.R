#' Convert to Expression
#'
#' Convert a string to an expression
#'
#' @param string (character) string to convert
#'
#' @return expression
convertToExpression <- function(string) {
  # Trim any leading or trailing whitespace
  trimmed_string <- trimws(string)

  # Return an empty string if the input is empty
  if (nchar(trimmed_string) == 0) {
    return("")
    # ggplot fails for empty expressions like: expression()
  }

  # Define a pattern to match complete subscripts and superscripts
  pattern <- "(\\w+\\[\\w+\\]|\\w+\\^\\w+)"

  # Identify matches for subscripts or superscripts
  matches <- gregexpr(pattern, trimmed_string)
  special_parts <- regmatches(trimmed_string, matches)[[1]]

  # Remove the special parts from the original string to get the literal text parts
  literal_parts <- unlist(strsplit(trimmed_string, pattern))

  # Rebuild the expression
  result_parts <- character(0)
  max_len <- max(length(literal_parts), length(special_parts))

  for (i in seq_len(max_len)) {
    if (i <= length(literal_parts) && nchar(literal_parts[i]) > 0) {
      result_parts <- c(result_parts, paste0('"', trimws(literal_parts[i]), '"'))
    }
    if (i <= length(special_parts) && nchar(special_parts[i]) > 0) {
      result_parts <- c(result_parts, special_parts[i])
    }
  }

  transformed_string <- paste(result_parts, collapse = " ~ ")

  # Convert the string to an expression
  expression_string <- paste0("expression(", transformed_string, ")")

  # parse the expression
  parsed_expression <- eval(parse(text = expression_string))

  return(parsed_expression)
}
