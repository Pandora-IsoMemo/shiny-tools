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
      label = "Legend position",
      choices = c("none", "right", "top", "bottom", "left", "custom"),
      width = width
    ),
    tags$br(),
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
    # pickerInput(
    #   inputId = ns("legendElements"),
    #   label = "Legend elements",
    #   choices = c("title"),
    #   multiple = TRUE,
    #   options = list(`actions-box` = TRUE,   # Enables the action box for select/deselect all
    #                  `none-selected-text` = "Select elements ..."),
    #   width = width
    # ),
    #formatTextUI(id = ns("legendText"), type = type)
    applyFormatUI(
      wrapper_id = ns("legend_layout"),
      format_FUN = formatTextUI,
      label_selected = "Legend elements",
      choices_selected = c("Add a point ..." = ""),
      id = ns("legend_layout-format"),
      type = type
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
    # legend position ----
    legend <- reactiveValues(position = "none",
                             layout = NULL)

    observe({
      logDebug("%s: Entering observe 'legendPosition'", id)
      if (input[["legendPosition"]] == "custom") {
        legend$position <- c(input[["legendX"]], input[["legendY"]])
      } else {
        legend$position <- input[["legendPosition"]]
      }
    })

    # legend content and layout ----
    default_layout <- config()$defaultGGLabel

    legend_element_choices <- reactive({
      if (length(legend_title()) == 1 && legend_title() != "") {
        choices <- c(legend_title())
      } else {
        choices <- c("title")
      }

      if (!is.null(legend_labels())) {
        choices <- c(choices, legend_labels())
      }

      choices
    })

    legend_elements <- reactiveVal(list())

    observe({
      logDebug("%s: Update 'legend_element()'", id)

      current_elements <- legend_elements()

      # create new elements from title and labels
      new_elements_names <- setdiff(legend_element_choices(), names(current_elements))
      new_elements <- setNames(replicate(length(new_elements_names), list(), simplify = FALSE), new_elements_names)
      # add NEW title or labels with default layout values to list
      new_elements <- new_elements %>%
        initFormat(default_format = default_layout)

      # set id as default text
      for (name in new_elements_names) {
        if (new_elements[[name]]$text == "") {
          new_elements[[name]]$text <- name
        }
      }

      # combine current and new elements
      all_elements <- c(current_elements, new_elements)

      # select all elements from title and labels
      legend_elements(all_elements[legend_element_choices()])
    }) %>%
      bindEvent(list(legend_title(), legend_labels()))

    observe({
      logDebug("%s: Update 'legend_layout'", id)
      legend$layout <- legend_elements()
    }) %>%
      bindEvent(legend_elements())

    applyFormatServer(
      id = "legend_layout",
      default_style = default_layout,
      formatServerFUN = formatTextServer,
      element_list = legend_elements,
      style_prefix = "",
      plot_type = plot_type,
      text_inputs = "show",
      position_inputs = "show",
      show_parse_button = FALSE
    )

    return(legend)
  })
}

# TEST MODULE -------------------------------------------------------------
# To test the module run devtools::load_all() first
# Please comment this code before building the package

ui <- fluidPage(
  tagList(
    navbarPage(
      header = includeShinyToolsCSS(),
      title = "test app",
      theme = shinythemes::shinytheme("flatly"),
      position = "fixed-top",
      collapsible = TRUE,
      id = "test"
    ),
    plotOutput("plot"),
    plotLegendUI(id = "testMod", title = "Legend", titleTag = "h4"),
    verbatimTextOutput("legend_layout")
  )
)

server <- function(input, output, session) {
  testPlotFun <- function() {
    data <- data.frame(
      x = c(1, 2, 3, 4, 5),
      y = c(3, 5, 2, 8, 7),
      group = factor(c("A", "B", "A", "B", "A"))
    )

    ggplot2::ggplot(data, ggplot2::aes(x = x, y = y, color = group)) +
      ggplot2::geom_line()
  }

  thisLegend <- plotLegendServer("testMod", legend_title = reactive("group"), legend_labels = reactive(c("A", "B")))

    output$legend_layout <- renderPrint({
      req(thisLegend$layout)
      thisLegend$layout %>% lapply(FUN = as.data.frame) %>% bind_rows(.id = "legend_element")
    })

  output$plot <- renderPlot({
    testPlotFun() %>%
      formatLegendOfGGplot(legend = thisLegend)
  })
}

shinyApp(ui = ui, server = server)
