#' Vector Input UI
#'
#'
#' @param id module id
#' @param newValueRange (numeric) Range of values that can be entered in the numeric input field
#' @inheritParams setModuleTitle
#'
#' @return tagList
#' @export
vectorInputUI <- function(id, newValueRange, title = NULL, titleTag = "strong") {
  ns <- NS(id)
  tagList(
    tags$hr(),
    div(
      style = "margin-bottom: 0.5em;",
      setModuleTitle(title = title, titleTag = titleTag)
    ),
    # Output field to display the current vector
    shiny::verbatimTextOutput(ns("current_vector")),
    # Dropdown to select the index of the vector element to update
    pickerInput(
      ns("index_select"),
      "Select indices:",
      width = "100%",
      choices = NULL,
      multiple = TRUE,
      options = list(`actions-box` = TRUE)  # Enables the action box for select/deselect all
    ),
    fluidRow(
      column(8,
             # Input field to enter a new value for the selected vector element
             numericInput(ns("new_value"),
                          "New value:",
                          value = NA,
                          min = newValueRange[1],
                          max = newValueRange[2],
                          width = "100%")),
      column(4,
             align = "right",
             style = "margin-top: 1.5em;",
             # Button to submit the new value
             actionButton(ns("submit_btn"), "Update"))
    ),
    tags$hr()
  )
}

#' Backend for Vector Input module
#'
#' Server function for handling the logic of the numeric vector input module
#'
#' @param id (character) The namespace identifier for the module
#' @param defaultInputs (reactive) Numeric vector containing the default values for the vector
#' @export
vectorInputServer <- function(id, defaultInputs) {
  moduleServer(id, function(input, output, session) {
    # Reactive value to store and manage updates to the vector
    vector <- reactiveVal()

    # Update vector whenever defaultInputs changes
    observe({
      vector(defaultInputs())

      index_choices <- seq_along(defaultInputs())
      names(index_choices) <- names(defaultInputs())
      # Ensure the picker input is updated with the new indices
      updatePickerInput(session, "index_select", choices = index_choices)
    })

    # Observe event on clicking the submit button to update the vector
    observeEvent(input$submit_btn, {
      # Ensure a value is entered before proceeding
      req(input$new_value, input$index_select)
      # Retrieve the current vector
      vec <- vector()
      indices <- as.numeric(input$index_select)
      # Update the vector at the selected index with the new value
      vec[indices] <- input$new_value
      # Save the updated vector back to the reactive value
      vector(vec)
    })

    # Render the text output displaying the current state of the vector
    output$current_vector <- renderText({
      vec <- vector()
      # Create a string representation of the named vector
      if (is.null(vec)) {
        "Vector is empty"
      } else {
        sprintf(
          "(%s)",
          paste(sapply(names(vec), function(n) paste(n, vec[n], sep = ": ")), collapse = ", ")
        )
      }
    })

    # Return the reactive vector for use elsewhere in the application
    return(reactive(vector()))
  })
}


# TEST MODULE -------------------------------------------------------------
# To test the module run devtools::load_all() first
# Please comment this code before building the package

# ui <- shiny::fluidPage(
#   vectorInputUI("vector_module", title = "Vector Input", newValueRange = c(0.0001, 100)),
#   numericInput("vector_size", "Vector Size", min = 1, max = 10, value = 5),
#   shiny::verbatimTextOutput("sum_output")
# )
#
# server <- function(input, output) {
#   # Create a reactive expression for defaultInputs based on input
#   defaultInputs <- reactive({
#     values <- seq_len(input$vector_size)
#     names(values) <- paste0("Element", seq_along(values))
#     values
#   })
#
#   # Initialize the module with reactive default inputs
#   vector_reactive <- vectorInputServer("vector_module", defaultInputs)
#
#   output$sum_output <- renderText({
#     vec <- vector_reactive()
#     if (is.null(vec)) "Vector is empty"
#     else paste("Sum of vector:", sum(vec))
#   })
# }
#
# shiny::shinyApp(ui = ui, server = server)
