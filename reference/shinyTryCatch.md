# Shiny Try Catch

Catch multiple errors and warnings, and forward them as an alert. If an
error occurs, NULL is returned. If a warning occurs, the result is
returned. Please, add
[`shinyjs::useShinyjs()`](https://rdrr.io/pkg/shinyjs/man/useShinyjs.html)
to the UI to enable the alerts.

## Usage

``` r
shinyTryCatch(
  expr,
  errorTitle = "Modeling failed",
  warningTitle = "",
  alertStyle = "shinyjs",
  inShiny = TRUE,
  suppressWarnings = FALSE
)
```

## Arguments

- expr:

  expression to be evaluated.

- errorTitle:

  (character) error message title.

- warningTitle:

  (character) warning message title.

- alertStyle:

  (character) Either "shinyjs", or "shinyalert". Specifies how an error
  or a warning is given out. If "shinyjs" than shinyjs::alert is used;
  if "shinyalert" than shinyalert::shinyalert is used.

- inShiny:

  (logical) if FALSE prevents an alert in shiny, but instead a warning
  will be displayed. Use this when not applied within a Shiny session.

- suppressWarnings:

  (logical) if TRUE, warnings are suppressed and not given out as an
  alert.
