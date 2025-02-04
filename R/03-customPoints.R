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
      tabPanel("Remove", removeCustomPointsUI(ns("remove"))),
      tabPanel(
        "Point",
        applyLayoutUI(
          id = ns("style_point"),
          layout_UI_FUN = plotPointsUI,
          label_selected = "Select point(s)",
          choices_selected = c("Add a point ..." = ""),
          title = NULL,
          type = plot_type,
          initStyle = config()$defaultPointStyle
        )
      ),
      # we need ui to style errors
      tabPanel(
        "Error",
        applyLayoutUI(
          id = ns("style_error"),
          layout_UI_FUN = formatLineUI,
          label_selected = "Select point(s)",
          choices_selected = c("Add a point ..." = ""),
          initStyle = config()$defaultLineStyle
        )
      ),
      tabPanel(
        "Label",
        applyLayoutUI(
          id = ns("style_label"),
          layout_UI_FUN = formatTextUI,
          label_selected = "Select point(s)",
          choices_selected = c("Add a point ..." = ""),
          type = plot_type,
          initStyle = config()$defaultGGLabel
        )
      )
    )
  )
}

#' Server function for custom points
#'
#' Backend for custom points module
#'
#' @param id namespace id
#' @param plot_type (character) Type of the plot to add points to, one of "ggplot", "base".
#' @param custom_points (reactiveVal) optional custom_points from parent module, this enables to
#'  change custom_points outside of the module
#'
#' @export
customPointsServer <- function(id,
                               plot_type = c("ggplot", "base", "none"),
                               custom_points = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (!("reactiveVal" %in% class(custom_points))) {
      custom_points <- reactiveVal()
    }

    addCustomPointServer("add", custom_points)
    removeCustomPointsServer("remove", custom_points)
    stylePointsServer("style_point", custom_points)
    applyLayoutServer(
      "style_error",
      default_style = defaultLineFormat(),
      layout_server_FUN = formatLineServer,
      element_list = custom_points,
      style_prefix = "error_",
      plot_type = plot_type
    )
    applyLayoutServer(
      "style_label",
      default_style = config()$defaultGGLabel,
      layout_server_FUN = formatTextServer,
      element_list = custom_points,
      style_prefix = "label_",
      plot_type = plot_type,
      id_as_text = TRUE,
      text_inputs = "show",
      position_inputs = "show",
      show_parse_button = FALSE
    )

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
          tags$br(),
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
      multiple = TRUE,
      width = "100%"
    ),
    tags$br(),
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
      last_selected <- input[["selected_points"]]
      updateSelectInput(
        session,
        "selected_points",
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
