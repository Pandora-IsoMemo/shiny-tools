# parent module ----
customPointsUI <- function(id, title = "Custom Points", titleTag = "h4") {
  ns <- NS(id)
  tagList(
    setModuleTitle(title = title, titleTag = titleTag),
    # tabs that contain panels: "add", "remove", "style"
    tabsetPanel(
      id = ns("custom_points"),
      selected = "Add",
      tabPanel("Add", addCustomPointUI(ns("add"))),
      tabPanel("Remove", removeCustomPointsUI(ns("remove"))),
      tabPanel("Style", styleCustomPointUI(ns("style")))
    )
  )
}

customPointsServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      custom_points <- reactiveVal()

      addCustomPointServer("add", custom_points = custom_points)
      removeCustomPointsServer("remove", custom_points = custom_points)
      #styleCustomPointServer(ns("style"), custom_points = custom_points)

      return(custom_points)
    }
  )
}

# sub modules ----
addCustomPointUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$br(),
    pointCoordinatesUI(ns("coordinates")),
    actionButton(ns("apply"), "Add point")
  )
}

addCustomPointServer <- function(id, custom_points = reactiveVal()) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      point_id <- reactiveVal(1)
      new_point <- pointCoordinatesServer("coordinates", default_name = reactive(paste("Point", point_id())))

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
        # this overwrites existing point
        all_points[[new_point()$id]] <- new_point()
        custom_points(all_points)

        # inc id
        point_id(point_id() + 1)
      }) %>%
        bindEvent(input[["apply"]])

      return(custom_points)
    }
  )
}

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

removeCustomPointsServer <- function(id, custom_points = reactiveVal()) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      observe({
        logDebug("%s: update 'input$pointsToRemove'", id)

        if (length(custom_points()) == 0) {
          new_choices <- c("Add a point ..." = "")
        } else {
          new_choices <- names(custom_points())
        }

        updateSelectInput(
          session,
          "pointsToRemove",
          choices = new_choices
        )
      })

      observeEvent(input[["apply"]], {
        logDebug("%s: Removing points", id)

        req(length(input[["pointsToRemove"]]) > 0)
        all_points <- custom_points()
        all_points <- all_points[!names(all_points) %in% input[["pointsToRemove"]]]
        custom_points(all_points)
      })

      return(custom_points)
    }
  )
}

styleCustomPointUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      ns("pointsToRemove"),
      label = "Select points to format",
      choices = c("Add a point ..." = ""),
      multiple = TRUE
    ),
    actionButton(ns("apply"), "Format point")
  )
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
#     custom_points()
#       })
# }
#
# shinyApp(ui, server)
