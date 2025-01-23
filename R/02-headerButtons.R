#' Add header buttons
#'
#' @param id module id
#' @param help_link link that is opened when help button is clicked
#' @param further_help_link link that is opened when further help button is clicked; if set to NULL, no button is displayed
#' @param loadShinyToolsCSS logical should shinyToolsCSS be loaded
#'
#' @return tagList with divs
#' @export
headerButtonsUI <- function(id, help_link, further_help_link = NULL, loadShinyToolsCSS = TRUE) {
  ns <- NS(id)

  tagList(
    tags$head(
      # add favicon for the browser tab
      tags$link(rel = "icon", type = "image/x-icon", href = "shinyTools_files/pandora-isomemo_favicon.ico"),
      # add css
      if (loadShinyToolsCSS) includeShinyToolsCSS() else NULL
    ),
    div(
      id = "header-right",
      div(
        id = "logo-pandora",
        tags$a(
          href = "https://pandoradata.earth/",
          img(src = "shinyTools_files/pandora-logo-white.png", alt = "Pandora"),
          target = "_blank"
        )
      ),
      div(
        id = "logo-isomemo",
        tags$a(
          href = "https://isomemo.gea.mpg.de/",
          img(src = "shinyTools_files/isomemo-logo-white.png", alt = "IsoMemo"),
          target = "_blank"
        )
      ),
      div(
        id = "help",
        tags$button(
          onclick = paste0("window.open('https://pandorasearch.earth/','_blank');"),
          class = "btn btn-default",
          "Data Search"
        )
      ),
      div(
        id = "help",
        tags$button(
          onclick = paste0("window.open('https://pandora-isomemo.github.io/docs/apps.html','_blank');"),
          class = "btn btn-default",
          "Apps"
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
#       title = "test app",
#       theme = shinythemes::shinytheme("flatly"),
#       position = "fixed-top",
#       collapsible = TRUE,
#       id = "test"
#     ),
#     headerButtonsUI(id = "header_id", help_link = "https://pandora-isomemo.github.io/shiny-tools/", further_help_link = NULL)
#   )
# )
#
# server <- function(input, output, session) {
# }
#
# shinyApp(ui = ui, server = server)
