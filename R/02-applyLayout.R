# UI for applying layout to selected elements
#
# @param layout_UI_FUN function. Function to create the UI for the layout.
# @param label_selected character. Label for the selectInput.
# @param choices_selected character. Choices for the selectInput.
# @param width character. Width of the selectInput.
# @param ... further arguments passed to the layout_UI_FUN.
#
# @rdname applyLayoutServer
applyLayoutUI <- function(id,
                          layout_UI_FUN,
                          label_selected = "Select point(s)",
                          choices_selected = c("Add a point ..." = ""),
                          width = "100%",
                          ...) {
  ns <- NS(id)

  tagList(
    helpText("Modify the layout."),
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
    layout_UI_FUN(id = ns("layout"), ...),
    tags$br(),
    actionButton(ns("apply"), "Apply")
  )
}

# Server logic for applying layout to selected elements
#
# @param id character. Module ID.
# @param default_style list. Default style settings.
# @param layout_server_FUN function. Function to create the server logic for the layout.
# @param element_list reactiveVal. List of elements to apply the layout to.
# @param style_prefix character. Prefix for the style settings.
# @param plot_type character. Type of plot. Default is "ggplot".
# @param layout_group reactive. Group of elements to always apply the same layout to.
# @param group_entries character. Specify layout entries that should be used for 'layout_group'.
#   If empty, all entries are updated for 'layout_group'.
# @param id_as_text (logical) if true and default_style has an entry "text", use the id from
#   element_list as default value
# @param ... further arguments passed to the layout_server_FUN.
applyLayoutServer <- function(id,
                              default_style,
                              layout_server_FUN,
                              element_list = reactiveVal(),
                              style_prefix = "",
                              plot_type = c("ggplot", "base", "none"),
                              layout_group = reactive(c()),
                              group_entries = c(),
                              id_as_text = FALSE,
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

      # add entries for label layout if not yet set
      all_elements <- element_list() %>%
        initLayout(default_layout = default_style, prefix = style_prefix, id_as_text = id_as_text)

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
        element_id("layout")
      }
    }) %>%
      bindEvent(input[["selected_elements"]], ignoreNULL = FALSE)

    init_style <- reactive({
      if (length(input[["selected_elements"]]) == 0 ||
          any(input[["selected_elements"]] == "")) {
        default_style
      } else {
        # load selected layout
        first_point_style <- element_list()[input[["selected_elements"]]][[1]] %>%
          extractFormat(prefix = style_prefix)
        first_point_style
      }
    })

    # current layout settings
    new_format <- layout_server_FUN(id = "layout",
                                  init_layout = init_style,
                                  element_id = element_id,
                                  ...)

    observe({
      logDebug("%s: Apply new layout", id)

      selected_elements <- input[["selected_elements"]]
      new_format <- new_format %>% extractReactiveValue()

      # apply layout to all selected elements and all entries
      all_elements <- element_list() %>%
        updateFormat(
          selected_ids = selected_elements,
          new_format = new_format,
          prefix = style_prefix
        )

      # apply layout to layout group and group_entries
      if (any(selected_elements %in% layout_group())) {
        logDebug("%s: Apply layout to layout group", id)
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

# Server function for styling custom points
#
# This function is analogue to applyLayoutServer, but plotPointsServer requires a different format
# of default values, which requires a separate function
#
# @param id namespace id
# @param custom_points reactiveVal
stylePointsServer <- function(id, custom_points = reactiveVal()) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # set default style
    default_style <- config()$defaultPointStyle["dataPoints"]

    # observe custom_points
    observe({
      logDebug("%s: update choices of 'input$selected_elements'", id)

      custom_point_ids <- names(custom_points())
      last_selected <- input[["selected_elements"]]
      updatePickerInput(
        session,
        "selected_elements",
        choices = getPointChoices(custom_point_ids),
        selected = last_selected
      )

      req(length(custom_point_ids) > 0)
      logDebug("%s: set point styles if empty", id)

      # add entries for point format if not yet set
      all_points <- custom_points() %>%
        initLayout(default_layout = default_style[["dataPoints"]], prefix = "point_")
      custom_points(all_points)
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

    reload_init <- reactiveVal(FALSE)
    observe({
      if (length(input[["selected_elements"]]) == 0 ||
          any(input[["selected_elements"]] == "")) {
        logDebug("%s: No init reload", id)
        reload_init(FALSE)
      } else {
        logDebug("%s: Reload init", id)
        reload_init(TRUE)
      }
    }) %>%
      bindEvent(input[["selected_elements"]], ignoreNULL = FALSE)

    init_style <- reactive({
      if (length(input[["selected_elements"]]) == 0 ||
          any(input[["selected_elements"]] == "")) {
        default_style
      } else {
        # load selected format
        first_point_style <- custom_points()[input[["selected_elements"]]][[1]] %>%
          extractFormat(prefix = "point_")
        list(dataPoints = first_point_style)
      }
    })

    style <- plotPointsServer(
      "layout",
      type = "ggplot",
      initStyle = init_style,
      reloadInit = reload_init
    )

    observe({
      logDebug("%s: Formatting points", id)

      all_points <- custom_points() %>%
        updateFormat(
          selected_ids = input[["selected_elements"]],
          new_format = style$dataPoints,
          prefix = "point_"
        )

      custom_points(all_points)
    }) %>%
      bindEvent(input[["apply"]])
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

initLayout <- function(elements, default_layout, prefix = "", id_as_text = FALSE) {
  # align names of entries
  names(default_layout) <- paste0(prefix, names(default_layout))

  ids <- names(elements)
  for (id in ids) {
    missing_entries <- setdiff(names(default_layout), names(elements[[id]]))
    for (name in missing_entries) {
      elements[[id]][[name]] <- default_layout[[name]]
      # set id as default text
      if (name == paste0(prefix, "text") && id_as_text) {
        elements[[id]][[name]] <- as.character(id)
      }
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
