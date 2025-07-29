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
#' @param x_choices (reactive) (named) character vector to provide choices if x is a categorical variable
#'
#' @export
customPointsServer <- function(id,
                               plot_type = c("ggplot", "base", "none"),
                               custom_points = NULL,
                               x_choices = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (!("reactiveVal" %in% class(custom_points))) {
      custom_points <- reactiveVal()
    }

    addCustomPointServer("add", custom_points, x_choices = x_choices)
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

    observe({
      if (!is.null(x_choices())) {
        shinyjs::hide(ns("style_error-layout-capheight"), asis = TRUE)
      } else {
        shinyjs::show(ns("style_error-layout-capheight"), asis = TRUE)
      }
    })

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
                                 reset_coordinates = TRUE,
                                 x_choices = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    point_counter <- reactiveVal(1)
    new_point <- pointCoordinatesServer("coordinates",
                                        default_name = reactive(paste("Point", point_counter())),
                                        reset_coordinates = reset_coordinates,
                                        x_choices = x_choices)

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
    helpText("Remove a point."),
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
#     # line plot
#     #ggplot(mtcars, aes(x = wt, y = mpg)) + geom_line()
#
#     # box plot
#     #ggplot(mtcars, aes(x = factor(cyl), y = mpg)) + ggplot2::geom_boxplot()
#
#     # density plot
#     #ggplot(mtcars, aes(x = cyl)) + ggplot2::geom_density()
#
#     # histogram
#     ggplot(mtcars, aes(x = cyl, fill = factor(.data$am))) +
#       ggplot2::geom_histogram(alpha = 0.5, binwidth = NULL, position = "identity")
#   }
#
#   # for line plot & histogram & density
#   custom_points <- customPointsServer("points")
#
#   # for boxplot
#   #custom_points <- customPointsServer("points", x_choices = reactive(structure(as.character(mtcars$cyl), x = "cyl")))
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
