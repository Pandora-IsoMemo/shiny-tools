#' Plot Legend UI
#'
#' Function to create the UI for the legend
#'
#' @inheritParams setModuleTitle
#' @param type character. Type of plot. Default is "ggplot".
#' @param width character. Width of the input.
#' @rdname plotLegendServer
#'
#' @export
plotLegendUI <- function(id, title = NULL, titleTag = "h4", type = c("ggplot", "base", "none"), width = "100%") {
  type <- match.arg(type)
  ns <- NS(id)
  tagList(
    setModuleTitle(title = title, titleTag = titleTag),
    selectInput(
      inputId = ns("legendPosition"),
      label = "Position",
      choices = c("none", "right", "top", "bottom", "left", "custom"),
      width = width
    ),
    conditionalPanel(
      ns = ns,
      condition = "input.legendPosition == 'custom'",
      sliderInput(
        inputId = ns("legendX"),
        label = "X position",
        value = 0.9,
        min = 0,
        max = 1,
        width = width
      ),
      sliderInput(
        inputId = ns("legendY"),
        label = "Y position",
        value = 0.1,
        min = 0,
        max = 1,
        width = width
      )
    ),
    selectInput(
      inputId = ns("legendDirection"),
      label = "Orientation",
      choices = c("auto", "vertical", "horizontal"),
      width = width
    ),
    tags$br(),
    applyFormatUI(
      wrapper_id = ns("legend_layout"),
      format_FUN = formatTextUI,
      label_selected = "Select legend item(s)",
      choices_selected = c("Add data ..." = ""),
      id = ns("legend_layout-format"),
      type = type,
      label_info_text = "The legend title and each legend label can be set individually.",
      layout_info_text = "The legend layout can only be set for the legend title or for all legend labels simultaneously."
    )
  )
}

#' Plot Legend Server
#'
#' Function to create the server for the legend
#'
#' @param id namespace id
#' @param legend_title reactive. Default title of the legend.
#' @param legend_labels reactive. Default labels of the legend.
#' @param plot_type character. Type of plot. Default is "ggplot".
#'
#' @export
plotLegendServer <- function(id, legend_title = reactive(NULL), legend_labels = reactive(NULL),
                             plot_type = c("ggplot", "base", "none")) {
  plot_type <- match.arg(plot_type)
  moduleServer(id, function(input, output, session) {
    legend <- reactiveValues(position = "none",
                             direction = "vertical",
                             layout = NULL)

    # legend position ----
    observe({
      logDebug("%s: Entering observe 'legendPosition'", id)
      if (input[["legendPosition"]] == "custom") {
        legend$position <- c(input[["legendX"]], input[["legendY"]])
      } else {
        legend$position <- input[["legendPosition"]]
      }
    })

    # legend direction ----
    observe({
      logDebug("%s: Entering observe 'legendDirection'", id)
      if (input[["legendDirection"]] == "auto") {
        legend$direction <- autoLegendDirection(input[["legendPosition"]])
      } else {
        legend$direction <- input[["legendDirection"]]
      }
    })

    # legend content and layout ----
    default_layout <- config()$defaultGGText

    legend_elements <- reactiveVal(list())

    observe({
      logDebug("%s: Update 'legend_element()'", id)

      current_elements <- legend_elements()

      # add NEW title element to current_elements
      title_id <- legend_title() %>% setLegendTitleID()
      if (!title_id %in% names(current_elements)) {
        current_elements[[title_id]] <- list(type = "title")
      }

      # add NEW label elements to current_elements
      label_ids <- as.character(legend_labels())
      if (length(label_ids) > 0) {
        for (label in label_ids) {
          if (!label %in% names(current_elements)) {
            current_elements[[label]] <- list(type = "label")
          }
        }
      }

      # initialize format for all NEW elements
      current_elements <- current_elements %>%
        initFormat(default_format = default_layout)

      # select only elements from title and labels arguments
      new_legend_choices <- getLegendChoices(title_id, label_ids)
      legend_elements(current_elements[new_legend_choices])
    }) %>%
      bindEvent(list(legend_title(), legend_labels()))

    observe({
      logDebug("%s: Update 'legend_layout'", id)

      legend$layout <- list(
        title = legend_elements() %>% filterType("title"),
        labels = legend_elements() %>% filterType("label")
      )
    }) %>%
      bindEvent(legend_elements())

    applyFormatServer(
      id = "legend_layout",
      default_style = default_layout,
      formatServerFUN = formatTextServer,
      element_list = legend_elements, # list that contains the layout for 'title' and the labels
      style_prefix = "",
      plot_type = plot_type,
      text_inputs = "show",
      position_inputs = "show",
      show_parse_button = FALSE,
      layout_group = reactive(legend_elements() %>% filterType("label") %>% names()), # same layout for all labels
      group_entries = names(default_layout)[!(names(default_layout) %in% c("text", "useExpression", "expression"))] # keep separate labels
    )

    return(legend)
  })
}

autoLegendDirection <- function(position) {
  if (position %in% c("top", "bottom")) {
    return("horizontal")
  } else {
    return("vertical")
  }
}

setLegendTitleID <- function(legend_title) {
  if (length(legend_title) == 1 && legend_title != "") {
    title <- legend_title
  } else {
    title <- "title"
  }

  title
}

getLegendChoices <- function(legend_title, legend_labels) {
  if (length(legend_title) == 1 && legend_title != "") {
    choices <- c(legend_title)
  } else {
    choices <- c("title")
  }

  if (!is.null(legend_labels)) {
    choices <- c(choices, legend_labels)
  }

  choices
}

filterType <- function(elements, type) {
  elements %>% Filter(f = function(x) x$type == type)
}

# TEST MODULE -------------------------------------------------------------
# To test the module run devtools::load_all() first
# Please comment this code before building the package

# ui <- fluidPage(shinyjs::useShinyjs(),
#   tagList(
#     navbarPage(
#       header = includeShinyToolsCSS(),
#       title = "test app",
#       theme = shinythemes::shinytheme("flatly"),
#       position = "fixed-top",
#       collapsible = TRUE,
#       id = "test"
#     ),
#     plotOutput("plot"),
#     plotLegendUI(id = "testMod", title = "Legend", titleTag = "h4"),
#     verbatimTextOutput("legend_layout")
#   )
# )
#
# server <- function(input, output, session) {
#   testPlotFun <- function() {
#     data <- data.frame(
#       x = c(1, 2, 3, 4, 5),
#       y = c(3, 5, 2, 8, 7),
#       group = factor(c("A", "B", "A", "B", "A"))
#     )
#
#     ggplot2::ggplot(data, ggplot2::aes(x = x, y = y, color = group)) +
#       ggplot2::geom_line()
#   }
#
#   thisLegend <- plotLegendServer("testMod", legend_title = reactive("group"), legend_labels = reactive(as.factor(c("A", "B"))))
#
#     output$legend_layout <- renderPrint({
#       req(thisLegend$layout$title)
#
#       title <- thisLegend$layout$title[[1]] %>% as.data.frame %>% mutate(element_id = names(thisLegend$layout$title))
#       labels <- thisLegend$layout$labels %>% lapply(FUN = as.data.frame) %>% bind_rows(.id = "element_id")
#
#       bind_rows(title, labels) %>%
#         select("element_id", dplyr::everything())
#     })
#
#   output$plot <- renderPlot({
#     testPlotFun() %>%
#       formatLegendOfGGplot(legend = thisLegend)
#   })
# }
#
# shinyApp(ui = ui, server = server)
