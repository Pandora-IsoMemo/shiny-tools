# Add header buttons

Add header buttons

## Usage

``` r
headerButtonsUI(
  id,
  help_link,
  further_help_link = NULL,
  loadShinyToolsCSS = TRUE
)
```

## Arguments

- id:

  module id

- help_link:

  link that is opened when help button is clicked

- further_help_link:

  link that is opened when further help button is clicked; if set to
  NULL, no button is displayed

- loadShinyToolsCSS:

  logical should shinyToolsCSS be loaded

## Value

tagList with divs
