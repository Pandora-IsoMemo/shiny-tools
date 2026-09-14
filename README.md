# shinyTools

<!-- badges: start -->
[![R-CMD-check](https://github.com/Pandora-IsoMemo/shiny-tools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Pandora-IsoMemo/shiny-tools/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/Pandora-IsoMemo/shiny-tools/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/Pandora-IsoMemo/shiny-tools/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

`shinyTools` is an R package containing reusable components for building
Shiny applications. It was developed for the Isomemo apps, but the modules
and helpers are designed to be shared across applications.

## What is included?

### Shiny modules

- **Plot controls:** configure titles, axis ranges, legends, and point styles
	for `ggplot2` and base R plots.
- **Plot export:** preview and download plots as PNG, JPEG, SVG, PDF, or TIFF;
	`ggplot2` exports can include the title and axis settings chosen by the user.
- **Data export:** download reactive data as CSV, XLSX, or JSON. XLSX exports
	can contain either one data frame or a workbook with multiple sheets.
- **Custom points:** add and manage user-defined points in plot-based Shiny
	workflows.
- **Vector input:** display a numeric vector and update selected elements from
	a Shiny input module.
- **Shared UI helpers:** add package styling, module titles, headers, font
	selectors, symbol selectors, and other common controls.

### Plot and application helpers

The package also provides functions for formatting `ggplot2` objects, including
scales, ranges, titles, legends, and point appearance. Additional helpers cover
reactive value resolution, updating user inputs, error handling in Shiny, and
plot/data export workflows.

## Installation

Install the development version from GitHub with:

```r
remotes::install_github("Pandora-IsoMemo/shiny-tools")
```

The package depends on Shiny, ggplot2, plotly, shinyWidgets, openxlsx, and
other packages listed in `DESCRIPTION`.

## Example

Shiny modules are connected with a shared `id` between their UI and server
functions. For example, a data export button can be added to an application as
follows:

```r
library(shiny)
library(shinyTools)

ui <- fluidPage(
	dataExportButton("data_export")
)

server <- function(input, output, session) {
	dataExportServer(
		"data_export",
		dataFun = reactive(function() mtcars),
		filename = "mtcars"
	)
}

shinyApp(ui, server)
```

See the [package documentation](https://pandora-isomemo.github.io/shiny-tools/)
for complete function references and module parameters.

## Documentation

- https://pandora-isomemo.github.io/shiny-tools/

## Release notes

- see `NEWS.md`
