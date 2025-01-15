pointCoordinatesUI <- function(id, title = "Custom Points", titleTag = "h4"){
  ns <- NS(id)
  tagList(
    setModuleTitle(title = title, titleTag = titleTag),
    pointDimensionUI("x", title = "X", titleTag = "h5"),
    pointDimensionUI("y", title = "Y", titleTag = "h5")
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

pointCoordinatesServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      reactive({
        list(
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
#   pointCoordinatesServer("points")
# }
#
# shinyApp(ui, server)
