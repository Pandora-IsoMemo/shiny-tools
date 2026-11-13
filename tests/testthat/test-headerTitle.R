test_that("headerTitle links to the news page with or without trailing slash", {
  expected <- "https://example.org/docu/news/index.html"

  for (u in c("https://example.org/docu/", "https://example.org/docu", " https://example.org/docu// ")) {
    html <- as.character(headerTitle("shiny", u))
    expect_match(html, expected, fixed = TRUE)
    expect_match(html, sprintf("v%s", as.character(packageVersion("shiny"))), fixed = TRUE)
    expect_match(html, 'rel="noopener noreferrer"', fixed = TRUE)
  }
})

test_that("headerTitle shows version without link if url is missing or invalid", {
  for (u in list(NULL, NA, "", "  ", 1, c("a", "b"))) {
    html <- as.character(headerTitle("shiny", u))
    expect_false(grepl("<a", html, fixed = TRUE))
    expect_match(
      paste(html, collapse = ""),
      sprintf("shiny(\\s|\\()+v%s", as.character(packageVersion("shiny")))
    )
  }
  expect_false(grepl("<a", as.character(headerTitle("shiny")), fixed = TRUE))
})

test_that("headerTitle returns only the name for an uninstalled package", {
  html <- as.character(headerTitle("notInstalledPkgXyz", "https://example.org/"))
  expect_equal(html, "notInstalledPkgXyz")
})

test_that("headerTitle validates pkg_name", {
  for (p in list(NULL, NA_character_, "", 1, c("shiny", "shinyTools"))) {
    expect_error(headerTitle(p), "`pkg_name` must be a single non-empty string")
  }
})
