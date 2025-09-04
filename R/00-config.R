config <- function() {
  config_path <- system.file("config.yaml", package = "shinyTools")
  if (nzchar(config_path)) {
    yaml.load_file(config_path)
  } else {
    stop("config.yaml not found via system.file(). Install/attach your package or set path.")
  }
}
