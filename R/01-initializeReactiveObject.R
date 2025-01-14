# initialize reactive values element that contains a list with sub lists based on default values


# Initialize reactive values
#
# Initialize reactiveValues object that will store user inputs for the axes or texts for
# the plotRanges or plotTitles module. The argument 'choices' specifies the (sub)set of choices to be initialized.
# If 'custom_values' is set, the default values are updated with the values from 'custom_values'.
#
# @param session session object from server function
# @param id module id
# @param custom_values (list) optional, named list with range definitions
# @param choices (character) Named vector of elements that will be available as user inputs.
# All elements must be contained in `names(default_fun())`.
# Names are used as labels for the user inputs, e.g. c("x axis" = "xAxis", "y axis" = "yAxis").
# @param default_fun (function) function that returns a named list with default values
# @param default_fun_args (list) optional, list of arguments to be passed to 'default_fun'
#
# @return reactiveValues
initializeReactiveObject <- function(session, id, custom_values, choices, default_fun, default_fun_args = list()) {
  default_values <- do.call(default_fun, default_fun_args)

  if (!all(choices %in% names(default_values))) {
    new_choices <- setdiff(choices, names(default_values))
    stop(sprintf("Logic for following choices does not exist yet: '%s'. Please check names of the choices! Currently, following choices are available: '%s'",
                 paste(setdiff(choices, new_choices), collapse = ", "),
                 paste(names(default_values), collapse = ", ")
    ))
  }

  # define reactiveValues object 'init_values' to store init_values for set 'choices'
  init_values <- do.call(reactiveValues, default_values[choices])

  # overwrite with custom initial init_values if set and if present in default_values
  if (!is.null(custom_values) && is.list(custom_values) && !is.reactivevalues(custom_values)) {

    # complete not reactive values:
    custom_values <- custom_values %>%
      completeValues(choices = choices, default_values = default_values)

    # make reactive values:
    init_values <- do.call(reactiveValues, custom_values)
  }

  if (!is.null(custom_values) && is.list(custom_values) && is.reactivevalues(custom_values)) {
    init_values <- custom_values

    # check (once!) if all choices are present, if not use default
    observe({
      logDebug("%s: Checking if all choices are present in user init_values", id)

      # complete reactive values inside observer:
      new_ranges <- init_values %>%
        completeValues(choices = choices, default_values = default_values)

      for (name in names(new_ranges)) {
        init_values[[name]] <- new_ranges[[name]]
      }
    }) %>%
      bindEvent(custom_values[[names(custom_values)[1]]], once = TRUE)
  }

  return(init_values)
}

completeValues <- function(custom_values, choices, default_values) {
  # complete ranges with needed entries that are not present in ranges
  missingEntries <- setdiff(choices, names(custom_values))

  # only add default values for missing entries that are present in default_values
  missingEntries <- intersect(missingEntries, names(default_values))

  for (name in missingEntries) {
    custom_values[[name]] <- default_values[[name]]
  }

  return(custom_values)
}
