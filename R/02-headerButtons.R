#' Add header buttons
#'
#' @param id module id
#' @param help_link link that is opened when help button is clicked
#'
#' @return tagList with divs
#' @export
headerButtonsUI <- function(id, help_link) {
  ns <- NS(id)
  tagList(
    div(
      id = "header-right",
      div(
        id = "logo-mpi",
        tags$a(
          href = "https://www.mpg.de/en",
          img(src = "app_files/MPIlogo.png", alt = "Supported by the Max Planck society"),
          target = "_blank"
        )
      ),
      div(
        id = "logo-isomemo",
        tags$a(
          href = "https://isomemo.com/",
          img(src = "app_files/IsoMemoLogo.png", alt = "IsoMemo"),
          target = "_blank"
        )
      ),
      div(
        id = "further-help",
        tags$button(
          onclick = "window.open('https://isomemo.com','_blank');",
          class = "btn btn-default",
          "Further Help"
        )
      ),
      div(
        id = "help",
        tags$button(
          onclick = paste0("window.open('",help_link,"','_blank');"),
          class = "btn btn-default",
          "?"
        )
      )
    )
  )
}


# TEST MODULE -------------------------------------------------------------
# To test the module run devtools::load_all() first
# Please comment this code before building the package

# ui <- fluidPage(
#   tagList(
#     navbarPage(
#       header = includeShinyToolsCSS(),
#       title = "test app",
#       theme = shinythemes::shinytheme("flatly"),
#       position = "fixed-top",
#       collapsible = TRUE,
#       id = "test"),
#     headerButtonsUI(id = "header_id", help_link = "https://www.inwt-statistics.de/")
#   )
# )
#
# server <- function(input, output, session) {
# }
#
# shinyApp(ui = ui, server = server)
