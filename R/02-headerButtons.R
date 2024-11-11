#' Add header buttons
#'
#' @param id module id
#' @param help_link link that is opened when help button is clicked
#' @param further_help_link link that is opened when further help button is clicked; if set to NULL, no button is displayed
#'
#' @return tagList with divs
#' @export
headerButtonsUI <- function(id, help_link, further_help_link = NULL) {
  ns <- NS(id)
  tagList(
    tags$head(
      # add favicon for the browser tab
      tags$link(rel = "icon", type = "image/x-icon", href = "https://github.com/Pandora-IsoMemo/docs/blob/main/img/pandora-isomemo_favicon.ico?raw=true"),
    ),
    div(
      id = "header-right",
      div(
        id = "logo-mpi",
        tags$a(
          href = "https://www.mpg.de/en",
          img(src = "https://github.com/Pandora-IsoMemo/docs/blob/main/img/mpi-minerva-logo.png?raw=true", alt = "Max Planck society"),
          target = "_blank"
        )
      ),
      div(
        id = "logo-isomemo",
        tags$a(
          href = "https://isomemo.com/",
          img(src = "https://github.com/Pandora-IsoMemo/docs/blob/main/img/isomemo-logo.png?raw=true", alt = "IsoMemo"),
          target = "_blank"
        )
      ),
      # further help nur wenn nicht NULL argument
      if (!is.null(further_help_link)) {
        div(
          id = "further-help",
          tags$button(
            onclick = paste0("window.open('", further_help_link, "','_blank');"),
            class = "btn btn-default",
            "Further Help"
          )
        )
      },
      div(
        id = "help",
        tags$button(
          onclick = paste0("window.open('", help_link, "','_blank');"),
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
#       id = "test"
#     ),
#     headerButtonsUI(id = "header_id", help_link = "https://isomemo.com/", further_help_link = NULL)
#   )
# )
#
# server <- function(input, output, session) {
# }
#
# shinyApp(ui = ui, server = server)
