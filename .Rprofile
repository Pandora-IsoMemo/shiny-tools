# do not push to git, it interferes with pipelines
if (as.logical(Sys.getenv("SHOW_DEBUG", unset = "FALSE"))) {
  library(futile.logger)
  futile.logger::flog.threshold(DEBUG)
}
