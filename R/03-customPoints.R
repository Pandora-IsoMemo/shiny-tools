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
                           titleTag = "h4") {
  ns <- NS(id)
  tagList(
    setModuleTitle(title = title, titleTag = titleTag),
    # tabs that contain panels: "add", "style", "remove"
    tabsetPanel(
      id = ns("custom_points"),
      selected = "Add",
      tabPanel("Add", addCustomPointUI(ns("add"))),
      tabPanel("Style", styleCustomPointsUI(ns("style"))),
      tabPanel("Remove", removeCustomPointsUI(ns("remove")))
    )
  )
}

#' Server function for custom points
#'
#' Backend for custom points module
#'
#' @param id namespace id
#'
#' @export
customPointsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    custom_points <- reactiveVal()

    addCustomPointServer("add", custom_points)
    styleCustomPointsServer("style", custom_points)
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
          actionButton(ns("apply"), "Add point"))
}

# Server function for adding custom points
# @param id namespace id
# @param custom_points reactiveVal
addCustomPointServer <- function(id, custom_points = reactiveVal()) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    point_counter <- reactiveVal(1)
    new_point <- pointCoordinatesServer("coordinates", default_name = reactive(paste("Point", point_counter())))

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
      # this overwrites existing point ----
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
      label = "Select points to remove",
      choices = c("Add a point ..." = ""),
      multiple = TRUE
    ),
    actionButton(ns("apply"), "Remove point")
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

# UI for styling custom points
# @param id namespace id
# @param plot_type (character) type of plot, one of "ggplot", "base", "none"
styleCustomPointsUI <- function(id, plot_type = c("ggplot", "base", "none")) {
  plot_type <- match.arg(plot_type)
  ns <- NS(id)

  tagList(
    tags$br(),
    selectInput(
      ns("pointsToStyle"),
      label = "Select points to style",
      choices = c("Add a point ..." = ""),
      multiple = TRUE
    ),
    formatTextUI(
      ns("text"),
      type = plot_type,
      initTitle = defaultTextFormat(type = plot_type)[["title"]],
      initAxis = defaultTextFormat(type = plot_type)[["text"]]
    ),
    actionButton(ns("apply"), "Format point")
  )
}

# Server function for styling custom points
# @param id namespace id
# @param custom_points reactiveVal
# @param plot_type (character) type of plot, one of "ggplot", "base", "none"
styleCustomPointsServer <- function(id,
                                    custom_points = reactiveVal(),
                                    plot_type = c("ggplot", "base", "none")) {
  plot_type <- match.arg(plot_type)
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    default_style <- defaultInitText(type = plot_type)[["plotTitle"]]

    observe({
      logDebug("%s: set custom_styles if empty, update 'input$pointsToStyle'",
               id)

      custom_point_ids <- names(custom_points())
      # update missing entries with default values
      new_points <- custom_points()
      for (id in custom_point_ids) {
        missingEntries <- setdiff(names(default_style), names(new_points[[id]]))
        for (name in missingEntries) {
          new_points[[id]][[name]] <- default_style[[name]]
        }
      }
      custom_points(new_points)

      # update choices
      if (length(custom_point_ids) == 0) {
        new_choices <- c("Add a point ..." = "")
      } else {
        new_choices <- custom_point_ids
      }
      updateSelectInput(session, "pointsToStyle", choices = new_choices)
    })

    # current format settings
    updated_text <- formatTextServer(
      "text",
      init_text = reactive(default_style),
      text_type = c("title", "axis"),
      show_parse_button = TRUE,
      label_name = reactive("plotTitle")
    )

    # disable button if nothing is selected
    observe({
      if (length(input[["pointsToStyle"]]) == 0 ||
          any(input[["pointsToStyle"]] == "")) {
        logDebug("%s: Disable button", id)
        shinyjs::disable(ns("apply"), asis = TRUE)
      } else {
        logDebug("%s: Enable button", id)
        shinyjs::enable(ns("apply"), asis = TRUE)
      }
    })

    observe({
      logDebug("%s: Formatting points", id)

      all_points <- custom_points()
      for (id in input[["pointsToStyle"]]) {
        for (entry in names(updated_text())) {
          all_points[[id]][[entry]] <- updated_text()[[entry]]
        }
      }

      custom_points(all_points)
    }) %>%
      bindEvent(input[["apply"]])
  })
}

# TEST MODULE -------------------------------------------------------------
# To test the module run devtools::load_all() first
# Please comment this code before building the package

# ui <- fluidPage(shinyjs::useShinyjs(),
#                 header = includeShinyToolsCSS(),
#                 customPointsUI("points"),
#                 tags$h3("output"),
#                 verbatimTextOutput("custom_points")
# )
#
# server <- function(input, output, session) {
#   custom_points <- customPointsServer("points")
#
#   output$custom_points <- renderPrint({
#     custom_points() %>% lapply(FUN = as.data.frame) %>% dplyr::bind_rows()
#   })
# }
#
# shinyApp(ui = ui, server = server)
