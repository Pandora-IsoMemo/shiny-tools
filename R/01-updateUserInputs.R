#' Update User Inputs
#'
#' Update values from 'input' with all values from userInputs. Entries that are NULL are removed.
#' Only updates entries that are present in 'input'.
#'
#' @param input input object from server function
#' @param output output object from server function
#' @param session session from server function
#' @param userInputs (list) named list of inputs to be updated
#'
#' @export
updateUserInputs <- function(input, output, session, userInputs) {
  # check if userInputs is a list
  if (!is.list(userInputs)) {
    warning("Update of user inputs failed. 'userInputs' must be a list!")
    return()
  }

  # remove NULL values, they cause upload of inputs to fail without warnings
  userInputs <- userInputs[!sapply(userInputs, is.null)]

  ## get and filter input names
  inputIDs <- names(userInputs)
  inputIDs <- inputIDs[inputIDs %in% names(input)]

  # update values
  for (i in seq_along(inputIDs)) {
    session$sendInputMessage(inputIDs[i], list(value = userInputs[[inputIDs[i]]]))
  }
}
