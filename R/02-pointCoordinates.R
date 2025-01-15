pointCoordinatesUI <- function(id, title = "Coordinates", titleTag = "h4"){
  ns <- NS(id)
  tagList(
    setModuleTitle(title = title, titleTag = titleTag),
    textInput(ns("label"), label = "Point Label", value = "Point 1"),
    pointDimensionUI(ns("x"), title = "X", titleTag = "h5"),
    pointDimensionUI(ns("y"), title = "Y", titleTag = "h5")
  )
}

pointDimensionUI <- function(id, title = NULL, titleTag = "h4"){
  ns <- NS(id)
  tagList(
    setModuleTitle(title = title, titleTag = titleTag),
    numericInput(
      ns("value"),
      label = "Value",
      value = numeric(0)
    ),
    numericInput(
      ns("min"),
      label = "Minimum",
      value = numeric(0)
    ),
    numericInput(
      ns("max"),
      label = "Maximum",
      value = numeric(0)
    )
  )
}

pointCoordinatesServer <- function(id, default_name = reactive(NULL)) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      observe({
        req(default_name())
        updateTextInput(session, "label", value = default_name())
      }) %>%
        bindEvent(default_name())

      reactive({
        list(
          id = input[["label"]],
          x = input[["x-value"]],
          x_min = input[["x-min"]],
          x_max = input[["x-max"]],
          y = input[["y-value"]],
          y_min = input[["y-min"]],
          y_max = input[["y-max"]]
        )
      })
    })
}

# TEST MODULE -------------------------------------------------------------
# To test the module run devtools::load_all() first
# Please comment this code before building the package

# ui <- fluidPage(
#   pointCoordinatesUI("points")
# )
#
# server <- function(input, output, session) {
#   pointCoordinatesServer("points", default_name = reactive("Point 10"))
# }
#
# shinyApp(ui, server)
