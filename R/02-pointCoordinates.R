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
    pointDimensionUI(ns("x"), label_value = "X Axis Value", label_range = "X Error (optional)"),
    pointDimensionUI(ns("y"), label_value = "Y Axis Value", label_range = "Y Error (optional)")
  )
}

pointDimensionUI <- function(id,
                             label_value = "Value",
                             label_range = "Error (optional)") {
  ns <- NS(id)
  tagList(
    numericInput(
      ns("value"),
      label = label_value,
      value = numeric(0),
      width = "100%"
    ),
    tags$label(label_range),
    numericInput(
      ns("min"),
      label = "Minimum",
      value = numeric(0),
      width = "100%"
    ),
    numericInput(
      ns("max"),
      label = "Maximum",
      value = numeric(0),
      width = "100%"
    )
  )
}

pointCoordinatesServer <- function(id,
                                   default_name = reactive(NULL),
                                   reset_coordinates = TRUE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      req(default_name())
      logDebug("%s: Update 'label'", id)
      updateTextInput(session, "label", value = default_name())

      if (reset_coordinates) {
        # clear inputs of pointDimensions
        updateNumericInput(session, "x-value", value = numeric(0))
        updateNumericInput(session, "x-min", value = numeric(0))
        updateNumericInput(session, "x-max", value = numeric(0))
        updateNumericInput(session, "y-value", value = numeric(0))
        updateNumericInput(session, "y-min", value = numeric(0))
        updateNumericInput(session, "y-max", value = numeric(0))
      }
    }) %>%
      bindEvent(default_name())

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
        new_point(
          list(
            id = input[["label"]],
            x = input[["x-value"]] %>% as.numeric(),
            y = input[["y-value"]] %>% as.numeric(),
            xmin = input[["x-min"]] %>% as.numeric(),
            xmax = input[["x-max"]] %>% as.numeric(),
            ymin = input[["y-min"]] %>% as.numeric(),
            ymax = input[["y-max"]] %>% as.numeric()
          )
        )
      }
    })

    new_point
  })
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
