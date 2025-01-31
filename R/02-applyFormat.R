#' UI for applying format to selected elements
#'
#' @param wrapper_id character. Wrapper ID for the UI.
#' @param format_FUN function. Function to create the UI for the format.
#' @param label_selected character. Label for the selectInput.
#' @param choices_selected character. Choices for the selectInput.
#' @param width character. Width of the selectInput.
#' @param ... further arguments passed to the format_FUN.
#'
#' @rdname applyFormatServer
applyFormatUI <- function(wrapper_id,
                          format_FUN,
                          label_selected = "Select point(s)",
                          choices_selected = c("Add a point ..." = ""),
                          width = "100%",
                          ...) {
  ns <- NS(wrapper_id)

  tagList(
    # selectInput(
    #   ns("selected_elements"),
    #   label = label_selected,
    #   choices = choices_selected,
    #   multiple = TRUE,
    #   width = width
    # ),
    pickerInput(
      inputId = ns("selected_elements"),  # Namespaced ID
      label = label_selected,             # Label for the input
      choices = choices_selected,         # Choices for selection
      multiple = TRUE,                     # Allow multiple selections
      options = pickerOptions(
        actionsBox = TRUE,                 # Enables "Select All" / "Deselect All"
        title = "Select items to enable button 'Apply' ..."    # Placeholder text
      ),
      width = width                      # Set custom width
    ),
    format_FUN(...),
    tags$br(),
    actionButton(ns("apply"), "Apply")
  )
}

#' Server logic for applying format to selected elements
#'
#' @param id character. Module ID.
#' @param default_style list. Default style settings.
#' @param formatServerFUN function. Function to create the server logic for the format.
#' @param element_list reactiveVal. List of elements to apply the format to.
#' @param style_prefix character. Prefix for the style settings.
#' @param plot_type character. Type of plot. Default is "ggplot".
#' @param layout_group reactive. Group of elements to apply the format to.
#' @param group_entries character. Specify entries for 'layout_group' to apply the format to.
#'   If empty, all entries are updated for 'layout_group'.
#' @param ... further arguments passed to the formatServerFUN.
applyFormatServer <- function(id,
                              default_style,
                              formatServerFUN,
                              element_list = reactiveVal(),
                              style_prefix = "",
                              plot_type = c("ggplot", "base", "none"),
                              layout_group = reactive(c()),
                              group_entries = c(),
                              ...) {
  plot_type <- match.arg(plot_type)
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      logDebug("%s: update choices of 'input$selected_elements'", id)

      custom_ids <- names(element_list())
      last_selected <- input[["selected_elements"]]
      updatePickerInput(
        session,
        "selected_elements",
        choices = getPointChoices(custom_ids),
        selected = last_selected
      )

      req(length(custom_ids) > 0)
      logDebug("%s: set label styles if empty", id)

      # add entries for label format if not yet set
      all_elements <- element_list() %>%
        initFormat(default_format = default_style, prefix = style_prefix)

      element_list(all_elements)
    })

    # disable button if nothing is selected
    observe({
      if (length(input[["selected_elements"]]) == 0 ||
          any(input[["selected_elements"]] == "")) {
        logDebug("%s: Disable button", id)
        shinyjs::disable(ns("apply"), asis = TRUE)
      } else {
        logDebug("%s: Enable button", id)
        shinyjs::enable(ns("apply"), asis = TRUE)
      }
    })

    element_id <- reactiveVal(FALSE)
    observe({
      if (length(input[["selected_elements"]]) == 0 ||
          any(input[["selected_elements"]] == "")) {
        logDebug("%s: No init reload", id)
        element_id(NULL)
      } else {
        logDebug("%s: Reload init", id)
        element_id("format")
      }
    }) %>%
      bindEvent(input[["selected_elements"]], ignoreNULL = FALSE)

    init_style <- reactive({
      if (length(input[["selected_elements"]]) == 0 ||
          any(input[["selected_elements"]] == "")) {
        default_style
      } else {
        # load selected format
        first_point_style <- element_list()[input[["selected_elements"]]][[1]] %>%
          extractFormat(prefix = style_prefix)
        first_point_style
      }
    })

    # current format settings
    new_format <- formatServerFUN(id = "format",
                                  init_layout = init_style,
                                  element_id = element_id,
                                  ...)

    observe({
      logDebug("%s: Apply new format", id)

      selected_elements <- input[["selected_elements"]]
      new_format <- new_format %>% extractReactiveValue()

      # apply format to all selected elements and all entries
      all_elements <- element_list() %>%
        updateFormat(
          selected_ids = selected_elements,
          new_format = new_format,
          prefix = style_prefix
        )

      # apply format to layout group and group_entries
      if (any(selected_elements %in% layout_group())) {
        logDebug("%s: Apply format to layout group", id)
        selected_elements <- unique(c(selected_elements, layout_group()))
        if (length(group_entries) == 0) {
          group_entries <- names(new_format)
        }

        all_elements <- all_elements %>%
          updateFormat(
            selected_ids = selected_elements,
            new_format = new_format[group_entries],
            prefix = style_prefix
          )
      }

      element_list(all_elements)
    }) %>%
      bindEvent(input[["apply"]])

    return(element_list)
  })
}

extractReactiveValue <- function(obj) {
  if ("reactiveVal" %in% class(obj) || "reactive" %in% class(obj)) {
    # For reactiveVal, call the object to get its value
    return(obj())
  } else if ("reactivevalues" %in% class(obj)) {
    # For reactiveValues, convert to a list
    return(reactiveValuesToList(obj))
  } else {
    stop("The provided object is neither a reactiveVal nor a reactiveValues.")
  }
}

getPointChoices <- function(custom_ids) {
  if (length(custom_ids) == 0) {
    new_choices <- c("Add a point ..." = "")
  } else {
    new_choices <- custom_ids
  }

  new_choices
}

initFormat <- function(elements, default_format, prefix = "") {
  # align names of entries
  names(default_format) <- paste0(prefix, names(default_format))

  ids <- names(elements)
  for (id in ids) {
    missing_entries <- setdiff(names(default_format), names(elements[[id]]))
    for (name in missing_entries) {
      elements[[id]][[name]] <- default_format[[name]]
    }
  }

  elements
}

extractFormat <- function(elements, prefix = "") {
  elements <- elements[grepl(paste0("^", prefix), names(elements))]
  names(elements) <- gsub(paste0("^", prefix), "", names(elements))
  elements
}

updateFormat <- function(elements,
                         selected_ids,
                         new_format,
                         prefix = "") {
  # align names of entries
  names(new_format) <- paste0(prefix, names(new_format))

  for (id in selected_ids) {
    for (entry in names(new_format)) {
      elements[[id]][[entry]] <- new_format[[entry]]
    }
  }

  elements
}
