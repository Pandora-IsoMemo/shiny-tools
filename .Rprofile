.First <- function() {
  options(
    repos = c(
      getOption("repos"),
      INWTLab = "https://inwtlab.github.io/drat/",
      PANDORA = "https://Pandora-IsoMemo.github.io/drat/"
    )
  )
}

.First()

if (as.logical(Sys.getenv("SHOW_DEBUG", unset = "FALSE"))) {
  library(futile.logger)
  futile.logger::flog.threshold(DEBUG)
}
