pointCoordinatesUI <- function(id) {
  ns <- NS(id)
  tagList(
    helpText(
      paste(
        "Enter the coordinates of a new point. Optionally, you can specify the error for the coordinates.",
        "The layout of the point, error bar, and label can be modified in the 'Point', 'Error', and 'Label' tabs, respectively."
      )
    ),
    textInput(
      ns("label"),
      label = "Point ID",
      value = "Point 1",
      width = "100%"
    ),
    uiOutput(ns("x_coordinate")),
    numericCoordinateUI(
      ns("y"),
      label_value = "Y Axis Value",
      label_min = "Min. Y (Optional Error)",
      label_max = "Max. Y (Optional Error)"
    )
  )
}

pointCoordinatesServer <- function(id,
                                   default_name = reactive(NULL),
                                   reset_coordinates = TRUE,
                                   x_choices = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      req(default_name())
      logDebug("%s: Update 'label'", id)
      updateTextInput(session, "label", value = default_name())

      if (reset_coordinates) {
        # clear inputs of pointDimensions
        if (is.null(x_choices())) {
          updateNumericInput(session, "x-value", value = numeric(0))
          updateNumericInput(session, "x-min", value = numeric(0))
          updateNumericInput(session, "x-max", value = numeric(0))
        } else {
          updateSelectInput(session, "x-value", selected = character(0))
        }
        updateNumericInput(session, "y-value", value = numeric(0))
        updateNumericInput(session, "y-min", value = numeric(0))
        updateNumericInput(session, "y-max", value = numeric(0))
      }
    }) %>%
      bindEvent(default_name())

    output$x_coordinate <- renderUI({
      logDebug("%s: Render 'x_coordinate' input", id)
      if (is.null(x_choices())) {
        numericCoordinateUI(
          ns("x"),
          label_value = "X Axis Value",
          label_min = "Min. X (Optional Error)",
          label_max = "Max. X (Optional Error)"
        )
      } else {
        categoricalCoordinateUI(
          ns("x"),
          choices = x_choices(),
          label_value = "X Group",
          selected = character(0)
        )
      }
    })

    new_point <- reactiveVal()

    observe({
      logDebug("%s: Update 'new_point'", id)
      if (length(names(input)) <= 1 ||
          any(is.na(sapply(c("x-value", "y-value"), function(name)
            input[[name]]))) ||
          input[["label"]] == "") {
        # if inputs are not yet initialized or missing
        new_point(NULL)
      } else {
        npnt <- list(
          id = input[["label"]],
          y = input[["y-value"]] %>% as.numeric(),
          ymin = input[["y-min"]] %>% as.numeric(),
          ymax = input[["y-max"]] %>% as.numeric()
        )

        if (is.null(x_choices())) {
          npnt$x <- input[["x-value"]] %>% as.numeric()
          npnt$xmin <- input[["x-min"]] %>% as.numeric()
          npnt$xmax <- input[["x-max"]] %>% as.numeric()
        } else {
          # we need both columns in the point object "x" and "attr(x_choices, "x")"
          npnt[[attr(x_choices(), "x")]] <- input[["x-value"]] %>% as.character()
          npnt$x <- input[["x-value"]] %>% as.character()
          npnt$xmin <- NA
          npnt$xmax <- NA
        }

        new_point(npnt)
      }
    })

    new_point
  })
}

numericCoordinateUI <- function(id,
                                label_value = "Value",
                                label_min = "Min. Error (optional)",
                                label_max = "Max. Error (optional)") {
  ns <- NS(id)
  tagList(
    numericInput(
      ns("value"),
      label = label_value,
      value = numeric(0),
      width = "100%"
    ),
    numericInput(
      ns("min"),
      label = label_min,
      value = numeric(0),
      width = "100%"
    ),
    numericInput(
      ns("max"),
      label = label_max,
      value = numeric(0),
      width = "100%"
    )
  )
}

categoricalCoordinateUI <- function(id,
                                    choices,
                                    label_value = "Group",
                                    selected = character(0)) {
  ns <- NS(id)
  tagList(
    selectInput(
      ns("value"),
      label = label_value,
      choices = choices,
      selected = selected,
      width = "100%"
    )
  )
}

# TEST MODULE -------------------------------------------------------------
# To test the module run devtools::load_all() first
# Please comment this code before building the package

# ui <- fluidPage(
#   pointCoordinatesUI("points"),
#   verbatimTextOutput("point")
# )
#
# server <- function(input, output, session) {
#   new_point <- pointCoordinatesServer("points", default_name = reactive("Point 10"))
#
#   output$point <- renderPrint({
#     new_point()
#   })
# }
#
# shinyApp(ui = ui, server = server)
