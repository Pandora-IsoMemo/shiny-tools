pointCoordinatesUI <- function(id,
                               title = NULL,
                               titleTag = "h4") {
  ns <- NS(id)
  tagList(
    setModuleTitle(title = title, titleTag = titleTag),
    textInput(ns("label"), label = "Point ID", value = "Point 1"),
    pointDimensionUI(ns("x"), title = "X Dimension", titleTag = "h4"),
    pointDimensionUI(ns("y"), title = "Y Dimension", titleTag = "h4")
  )
}

pointDimensionUI <- function(id,
                             title = NULL,
                             titleTag = "h4") {
  ns <- NS(id)
  tagList(
    setModuleTitle(title = title, titleTag = titleTag),
    numericInput(ns("value"), label = "Value", value = numeric(0)),
    numericInput(ns("min"), label = "Minimum", value = numeric(0)),
    numericInput(ns("max"), label = "Maximum", value = numeric(0))
  )
}

pointCoordinatesServer <- function(id, default_name = reactive(NULL), reset_coordinates = TRUE) {
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
            x = input[["x-value"]],
            y = input[["y-value"]],
            xmin = input[["x-min"]],
            xmax = input[["x-max"]],
            ymin = input[["y-min"]],
            ymax = input[["y-max"]]
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
