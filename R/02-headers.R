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
      tags$link(rel = "icon", type = "image/x-icon", href = "/shinyTools_files/pandora-isomemo_favicon.ico"),
      # add css
      if (loadShinyToolsCSS) includeShinyToolsCSS() else NULL
    ),
    div(
      id = "header-right",
      div(
        id = "logo-pandora-icon",
        tags$a(
          href = "https://pandoradata.earth/",
          img(src = "shinyTools_files/pandora-logo-white-icon.png", alt = "Pandora"),
          target = "_blank"
        )
      ),
      div(
        id = "logo-isomemo-icon",
        tags$a(
          href = "https://isomemo.gea.mpg.de/",
          img(src = "shinyTools_files/isomemo-logo-white-icon.png", alt = "IsoMemo"),
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

#' Get header title
#'
#' @param pkg_name name of the package
#' @param title title to display. If empty, the package name is used.
#' @param url_docu HTTP(S) base URL to package documentation, e.g.
#'   "https://pandora-isomemo.github.io/data-tools/". Must contain a hostname,
#'   with an optional port (0-65535) and path, but no query or fragment.
#'   If missing or invalid, the version is shown without a link.
#'
#' @return A [shiny::tagList()]. If the package is not installed, only the package name is shown.
#' @export
headerTitle <- function(pkg_name, title = "", url_docu = NULL) {
  if (!is.character(pkg_name) || length(pkg_name) != 1 || is.na(pkg_name) || !nzchar(pkg_name)) {
    stop("`pkg_name` must be a single non-empty string.", call. = FALSE)
  }

  if (!is.character(title) || length(title) != 1 || is.na(title)) {
    stop("`title` must be a single string.", call. = FALSE)
  } else {
    title <- trimws(title)
  }

  if (!nzchar(title)) {
    title <- pkg_name
  }

  version <- tryCatch(
    as.character(packageVersion(pkg_name)),
    error = function(e) NULL
  )

  if (is.null(version)) {
    return(tagList(title))
  }

  version_label <- sprintf("v%s", version)

  has_url <- is.character(url_docu) && length(url_docu) == 1 &&
    !is.na(url_docu) && grepl(
      paste0(
        "^https?://[[:alnum:]]([[:alnum:]-]*[[:alnum:]])?",
        "(\\.[[:alnum:]]([[:alnum:]-]*[[:alnum:]])?)*",
        "(:[0-9]+)?(/[^[:space:]?#]*)?$"
      ),
      trimws(url_docu),
      ignore.case = TRUE
    )

  if (has_url) {
    authority <- sub("^https?://([^/]+).*$", "\\1", trimws(url_docu), ignore.case = TRUE)
    if (grepl(":", authority, fixed = TRUE)) {
      port <- as.numeric(sub(".*:", "", authority))
      has_url <- port <= 65535
    }
  }

  version_tag <- if (has_url) {
    tags$a(
      version_label,
      href = paste0(sub("/+$", "", trimws(url_docu)), "/news/index.html"),
      target = "_blank",
      rel = "noopener noreferrer"
    )
  } else {
    version_label
  }

  tagList(title, " ", version_tag)
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
