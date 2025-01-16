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
      tabPanel("Style", styleCustomPointsUI(ns("style")))
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
      styleCustomPointsServer("style", custom_points = custom_points)

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

styleCustomPointsUI <- function(id, type = c("ggplot", "base", "none")) {
  ns <- NS(id)
  tagList(
    tags$br(),
    selectInput(
      ns("pointsToStyle"),
      label = "Select points to style",
      choices = c("Add a point ..." = ""),
      multiple = TRUE
    ),
    # ui to style points ...
    formatTextUI(
      ns("text"),
      type = type,
      initTitle = defaultTextFormat(type = type)[["title"]],
      initAxis = defaultTextFormat(type = type)[["text"]]
    ),
    actionButton(ns("apply"), "Format point")
  )
}

styleCustomPointsServer <- function(id, custom_points = reactiveVal()) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      observe({
        logDebug("%s: update 'input$pointsToStyle'", id)

        if (length(custom_points()) == 0) {
          new_choices <- c("Add a point ..." = "")
        } else {
          new_choices <- names(custom_points())
        }

        updateSelectInput(
          session,
          "pointsToStyle",
          choices = new_choices
        )
      })

      init_text <- reactiveVal()
      observe({
        logDebug("%s: Entering observe 'input$labelName'", id)

        if (is.null(input[["labelName"]]) || input[["labelName"]] == "") {
          init_text(plotText[[names(plotText)[1]]])
        } else {
          init_text(plotText[[input[["labelName"]]]])
        }
      }) %>%
        bindEvent(input[["labelName"]])

      updated_text <- formatTextServer("text",
                                       init_text = defaultInitText(type = c("none", "ggplot", "base"),
                                                                   availableElements = c("title", "axis")),
                                       text_type = c("title", "axis"),
                                       show_parse_button = TRUE,
                                       label_name = reactive(input[["labelName"]]))


      observeEvent(input[["apply"]], {
        logDebug("%s: Formatting points", id)

        req(length(input[["pointsToStyle"]]) > 0)
        all_points <- custom_points()

        # logic to style points ...

        custom_points(all_points)
      })

      return(custom_points)
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
# shinyApp(ui, server)
