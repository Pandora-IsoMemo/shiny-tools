config <- function() {
  config_path <- system.file("config.yaml", package = "shinyTools")
  yaml.load_file(config_path)
}
