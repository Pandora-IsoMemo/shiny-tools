# parent module ----

#' Custom Points Module
#'
#' This module provides the UI that allows to add, style and remove custom points, and returns the
#'  config list for the custom points.
#'
#' @inheritParams setModuleTitle
#' @rdname customPointsServer
#'
#' @export
customPointsUI <- function(id,
                           title = "Custom Points",
                           titleTag = "h4",
                           plot_type = c("ggplot", "base", "none")) {
  plot_type <- match.arg(plot_type)

  ns <- NS(id)
  tagList(
    setModuleTitle(title = title, titleTag = titleTag),
    # tabs that contain panels: "add", "style", "remove"
    tabsetPanel(
      id = ns("custom_points"),
      selected = "Add",
      tabPanel("Add", addCustomPointUI(ns("add"))),
      tabPanel("Format Point", applyFormatUI(
        wrapper_id = ns("style_point"),
        format_FUN = plotPointsUI,
        id = ns("style_point-format"),
        title = NULL,
        type = plot_type,
        initStyle = config()$defaultPointStyle
      )),
      # we need ui to style errors
      tabPanel("Format Error", applyFormatUI(
        wrapper_id = ns("style_error"),
        format_FUN = formatLineUI,
        id = ns("style_error-format"),
        initStyle = config()$defaultLineStyle
      )),
      tabPanel("Format Label", applyFormatUI(
        wrapper_id = ns("style_label"),
        format_FUN = formatTextUI,
        id = ns("style_label-format"),
        type = plot_type,
        initTitle = defaultTextFormat(type = plot_type)[["title"]],
        initAxis = defaultTextFormat(type = plot_type)[["text"]]
      )),
      tabPanel("Remove", removeCustomPointsUI(ns("remove")))
    )
  )
}

#' Server function for custom points
#'
#' Backend for custom points module
#'
#' @param id namespace id
#' @param plot_type (character) Type of the plot to add points to, one of "ggplot", "base".
#'
#' @export
customPointsServer <- function(id, plot_type = c("ggplot", "base", "none")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    custom_points <- reactiveVal()

    addCustomPointServer("add", custom_points)
    stylePointsServer("style_point", custom_points)
    applyFormatServer("style_error",
                      default_style = defaultLineFormat(),
                      formatServerFUN = formatLineServer,
                      custom_points = custom_points,
                      style_prefix = "error_",
                      plot_type = plot_type)
    stylePointLabelsServer("style_label", custom_points)
    removeCustomPointsServer("remove", custom_points)

    return(custom_points)
  })
}

# sub modules ----

# UI for adding custom points
# @param id namespace id
addCustomPointUI <- function(id) {
  ns <- NS(id)
  tagList(tags$br(),
          pointCoordinatesUI(ns("coordinates")),
          actionButton(ns("apply"), "Add"))
}

# Server function for adding custom points
# @param id namespace id
# @param custom_points reactiveVal
addCustomPointServer <- function(id,
                                 custom_points = reactiveVal(),
                                 reset_coordinates = TRUE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    point_counter <- reactiveVal(1)
    new_point <- pointCoordinatesServer("coordinates",
                                        default_name = reactive(paste("Point", point_counter())),
                                        reset_coordinates = reset_coordinates)

    # disable button if new_point is not complete
    observe({
      if (length(new_point()) == 0) {
        logDebug("%s: Disable button", id)
        shinyjs::disable(ns("apply"), asis = TRUE)
      } else {
        logDebug("%s: Enable button", id)
        shinyjs::enable(ns("apply"), asis = TRUE)
      }
    })

    observe({
      logDebug("%s: Adding new point", id)

      req(length(new_point()) > 0)
      all_points <- custom_points()
      # OVERWRITES existing point ----
      all_points[[new_point()$id]] <- new_point()
      custom_points(all_points)

      # inc id
      point_counter(point_counter() + 1)
    }) %>%
      bindEvent(input[["apply"]])
  })
}

# UI for removing custom points
# @param id namespace id
removeCustomPointsUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$br(),
    selectInput(
      ns("pointsToRemove"),
      label = "Select point(s)",
      choices = c("Add a point ..." = ""),
      multiple = TRUE
    ),
    actionButton(ns("apply"), "Remove")
  )
}

# Server function for removing custom points
# @param id namespace id
# @param custom_points reactiveVal
removeCustomPointsServer <- function(id, custom_points = reactiveVal()) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      logDebug("%s: update 'input$pointsToRemove'", id)

      if (length(custom_points()) == 0) {
        new_choices <- c("Add a point ..." = "")
      } else {
        new_choices <- names(custom_points())
      }

      updateSelectInput(session, "pointsToRemove", choices = new_choices)
    })

    observeEvent(input[["apply"]], {
      logDebug("%s: Removing points", id)

      req(length(input[["pointsToRemove"]]) > 0)
      all_points <- custom_points()
      all_points <- all_points[!names(all_points) %in% input[["pointsToRemove"]]]
      custom_points(all_points)
    })

    return(custom_points)
  })
}

# UI for applying format to custom points
#
# @param thisId namespace id of the wrapper module 'applyFormatUI'
# @param formatUIFUN UI function for formatting
applyFormatUI <- function(wrapper_id, format_FUN, ...) {
  ns <- NS(wrapper_id)

  tagList(
    tags$br(),
    selectInput(
      ns("selected_points"),
      label = "Select point(s)",
      choices = c("Add a point ..." = ""),
      multiple = TRUE,
      width = "100%"
    ),
    format_FUN(...),
    actionButton(ns("apply"), "Apply")
  )
}

applyFormatServer <- function(id,
                              default_style,
                              formatServerFUN,
                              custom_points = reactiveVal(),
                              style_prefix = "",
                              plot_type = c("ggplot", "base", "none"),
                              ...) {
  plot_type <- match.arg(plot_type)
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    #default_style <- defaultInitText(type = plot_type)[["xAxisText"]]

    observe({
      logDebug("%s: update choices of 'input$selected_points'", id)

      custom_point_ids <- names(custom_points())
      updateSelectInput(session, "selected_points", choices = getPointChoices(custom_point_ids))

      req(length(custom_point_ids) > 0)
      logDebug("%s: set label styles if empty", id)

      # add entries for label format if not yet set
      all_points <- custom_points() %>%
        initFormat(default_format = default_style, prefix = style_prefix)

      custom_points(all_points)
    })

    # disable button if nothing is selected
    observe({
      if (length(input[["selected_points"]]) == 0 ||
          any(input[["selected_points"]] == "")) {
        logDebug("%s: Disable button", id)
        shinyjs::disable(ns("apply"), asis = TRUE)
      } else {
        logDebug("%s: Enable button", id)
        shinyjs::enable(ns("apply"), asis = TRUE)
      }
    })


    reload_init <- reactiveVal(FALSE)
    observe({
      if (length(input[["selected_points"]]) == 0 ||
          any(input[["selected_points"]] == "")) {
        logDebug("%s: No init reload", id)
        reload_init(FALSE)
      } else {
        logDebug("%s: Reload init", id)
        reload_init(TRUE)
      }
    }) %>%
      bindEvent(input[["selected_points"]], ignoreNULL = FALSE)

    init_style <- reactive({
      if (length(input[["selected_points"]]) == 0 ||
          any(input[["selected_points"]] == "")) {
        default_style
      } else {
        # load selected format
        first_point_style <- custom_points()[input[["selected_points"]]][[1]] %>%
          extractFormat(prefix = style_prefix)
        first_point_style
      }
    })

    # current format settings
    new_format <- formatServerFUN(
      id = "format",
      initStyle = init_style,
      reloadInit = reload_init,
      ...
    )

    observe({
      logDebug("%s: Apply new format", id)

      all_points <- custom_points() %>%
        updateFormat(
          selected_ids = input[["selected_points"]],
          new_format = new_format %>% extractReactiveValue(),
          prefix = style_prefix
        )

      custom_points(all_points)
    }) %>%
      bindEvent(input[["apply"]])
  })
}

extractReactiveValue <- function(obj) {
  if ("reactiveVal" %in% class(obj)) {
    # For reactiveVal, call the object to get its value
    return(obj())
  } else if ("reactivevalues" %in% class(obj)) {
    # For reactiveValues, convert to a list
    return(reactiveValuesToList(obj))
  } else {
    stop("The provided object is neither a reactiveVal nor a reactiveValues.")
  }
}

# Server function for styling custom points
# @param id namespace id
# @param custom_points reactiveVal
stylePointsServer <- function(id, custom_points = reactiveVal()) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # set default style
    default_style <- config()$defaultPointStyle["dataPoints"]

    # observe custom_points
    observe({
      logDebug("%s: update choices of 'input$selected_points'", id)

      custom_point_ids <- names(custom_points())
      updateSelectInput(session, "selected_points", choices = getPointChoices(custom_point_ids))

      req(length(custom_point_ids) > 0)
      logDebug("%s: set point styles if empty", id)

      # add entries for point format if not yet set
      all_points <- custom_points() %>%
        initFormat(default_format = default_style[["dataPoints"]], prefix = "point_")
      custom_points(all_points)
    })

    # disable button if nothing is selected
    observe({
      if (length(input[["selected_points"]]) == 0 ||
          any(input[["selected_points"]] == "")) {
        logDebug("%s: Disable button", id)
        shinyjs::disable(ns("apply"), asis = TRUE)
      } else {
        logDebug("%s: Enable button", id)
        shinyjs::enable(ns("apply"), asis = TRUE)
      }
    })

    reload_init <- reactiveVal(FALSE)
    observe({
      if (length(input[["selected_points"]]) == 0 ||
          any(input[["selected_points"]] == "")) {
        logDebug("%s: No init reload", id)
        reload_init(FALSE)
      } else {
        logDebug("%s: Reload init", id)
        reload_init(TRUE)
      }
    }) %>%
      bindEvent(input[["selected_points"]], ignoreNULL = FALSE)

    init_style <- reactive({
      if (length(input[["selected_points"]]) == 0 ||
          any(input[["selected_points"]] == "")) {
        default_style
      } else {
        # load selected format
        first_point_style <- custom_points()[input[["selected_points"]]][[1]] %>%
          extractFormat(prefix = "point_")
        list(dataPoints = first_point_style)
      }
    })

    style <- plotPointsServer(
      "format",
      type = "ggplot",
      initStyle = init_style,
      reloadInit = reload_init
    )

    observe({
      logDebug("%s: Formatting points", id)

      all_points <- custom_points() %>%
        updateFormat(
          selected_ids = input[["selected_points"]],
          new_format = style$dataPoints,
          prefix = "point_"
        )

      custom_points(all_points)
    }) %>%
      bindEvent(input[["apply"]])
  })
}

stylePointErrorsServer <- function(id, custom_points = reactiveVal(),
                                   plot_type = c("ggplot", "base", "none")) {
  plot_type <- match.arg(plot_type)
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

  })
}

# Server function for styling custom point labels
# @param id namespace id
# @param custom_points reactiveVal
# @param plot_type (character) type of plot, one of "ggplot", "base", "none"
stylePointLabelsServer <- function(id,
                                   custom_points = reactiveVal(),
                                   plot_type = c("ggplot", "base", "none")) {
  plot_type <- match.arg(plot_type)
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    default_style <- defaultInitText(type = plot_type)[["xAxisText"]]

    observe({
      logDebug("%s: update choices of 'input$selected_points'", id)

      custom_point_ids <- names(custom_points())
      updateSelectInput(session, "selected_points", choices = getPointChoices(custom_point_ids))

      req(length(custom_point_ids) > 0)
      logDebug("%s: set label styles if empty", id)

      # add entries for label format if not yet set
      all_points <- custom_points() %>%
        initFormat(default_format = default_style, prefix = "label_")

      # set id as default text
      for (name in custom_point_ids) {
        if (all_points[[name]]$label_text == "") {
          all_points[[name]]$label_text <- name
        }
      }

      custom_points(all_points)
    })

    # disable button if nothing is selected
    observe({
      if (length(input[["selected_points"]]) == 0 ||
          any(input[["selected_points"]] == "")) {
        logDebug("%s: Disable button", id)
        shinyjs::disable(ns("apply"), asis = TRUE)
      } else {
        logDebug("%s: Enable button", id)
        shinyjs::enable(ns("apply"), asis = TRUE)
      }
    })

    label_name <- reactiveVal("xAxisText")
    init_text <- reactive({
      if (length(input[["selected_points"]]) == 0 ||
          any(input[["selected_points"]] == "")) {
        default_style
      } else {
        # trigger label_name to force "updateUserInputs"
        label_name(NULL)
        label_name("xAxisText")
        # load selected format
        first_point_style <- custom_points()[input[["selected_points"]]][[1]] %>%
          extractFormat(prefix = "label_")
        first_point_style
      }
    })

    # current format settings
    updated_text <- formatTextServer(
      "format",
      init_text = init_text,
      text_type = c("title", "axis"),
      show_parse_button = FALSE,
      label_name = label_name,
      text_inputs = "show",
      position_inputs = "show"
    )

    observe({
      logDebug("%s: Formatting point labels", id)

      all_points <- custom_points() %>%
        updateFormat(
          selected_ids = input[["selected_points"]],
          new_format = updated_text(),
          prefix = "label_"
        )

      custom_points(all_points)
    }) %>%
      bindEvent(input[["apply"]])
  })
}

getPointChoices <- function(custom_point_ids) {
  if (length(custom_point_ids) == 0) {
    new_choices <- c("Add a point ..." = "")
  } else {
    new_choices <- custom_point_ids
  }

  new_choices
}

initFormat <- function(points, default_format, prefix = "") {
  # align names of entries
  names(default_format) <- paste0(prefix, names(default_format))

  custom_point_ids <- names(points)
  for (id in custom_point_ids) {
    missing_entries <- setdiff(names(default_format), names(points[[id]]))
    for (name in missing_entries) {
      points[[id]][[name]] <- default_format[[name]]
    }
  }

  points
}

extractFormat <- function(points, prefix = "") {
  points <- points[grepl(paste0("^", prefix), names(points))]
  names(points) <- gsub(paste0("^", prefix), "", names(points))
  points
}

updateFormat <- function(points, selected_ids, new_format, prefix = "") {
  # align names of entries
  names(new_format) <- paste0(prefix, names(new_format))

  for (id in selected_ids) {
    for (entry in names(new_format)) {
      points[[id]][[entry]] <- new_format[[entry]]
    }
  }

  points
}

# TEST MODULE -------------------------------------------------------------
# To test the module run devtools::load_all() first
# Please comment this code before building the package

# ui <- fluidPage(
#   shinyjs::useShinyjs(),
#   header = includeShinyToolsCSS(),
#   customPointsUI("points"),
#   tags$h3("output"),
#   plotOutput("plot"),
#   verbatimTextOutput("custom_points")
# )
#
# server <- function(input, output, session) {
#   testPlotFun <- function() {
#     ggplot(mtcars, aes(x = wt, y = mpg)) + geom_line()
#   }
#
#   custom_points <- customPointsServer("points")
#
#   output$custom_points <- renderPrint({
#     custom_points() %>% lapply(FUN = as.data.frame) %>% bind_rows()
#   })
#
#   output$plot <- renderPlot({
#     testPlotFun() %>%
#       addCustomPointsToGGplot(custom_points = custom_points())
#   })
# }
#
# shinyApp(ui = ui, server = server)
