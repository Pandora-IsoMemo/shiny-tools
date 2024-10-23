#' Update User Inputs
#'
#' Update values from 'input' with all values from userInputs
#'
#' @param input input object from server function
#' @param output output object from server function
#' @param session session from server function
#' @param userInputs (list) named list of inputs to be updated
#'
#' @export
updateUserInputs <- function(input, output, session, userInputs) {
  ## get and filter input names
  inputIDs <- names(userInputs)
  inputIDs <- inputIDs[inputIDs %in% names(input)]

  # update values
  for (i in 1:length(inputIDs)) {
    session$sendInputMessage(inputIDs[i], list(value = userInputs[[inputIDs[i]]]))
  }
}
