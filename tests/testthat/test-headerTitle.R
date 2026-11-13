test_that("headerTitle links to the news page with or without trailing slash", {
  expected <- "https://example.org/docu/news/index.html"

  for (u in c("https://example.org/docu/", "https://example.org/docu", " https://example.org/docu// ")) {
    html <- as.character(headerTitle("shiny", url_docu = u))
    expect_match(html, expected, fixed = TRUE)
    expect_match(html, sprintf("v%s", as.character(packageVersion("shiny"))), fixed = TRUE)
    expect_match(html, 'rel="noopener noreferrer"', fixed = TRUE)
  }
})

test_that("headerTitle shows version without link if url is missing or invalid", {
  for (u in list(
    NULL, NA, "", "  ", 1, c("a", "b"),
    "not a URL", "example.org/docu", "ftp://example.org/docu",
    "https://", "https:///docu", "https://example .org/docu",
    "https://example.org/docu?lang=en", "https://example.org/docu#section"
  )) {
    html <- as.character(headerTitle("shiny", url_docu = u))
    expect_false(grepl("<a", html, fixed = TRUE))
    expect_match(
      paste(html, collapse = ""),
      sprintf("shiny(\\s|\\()+v%s", as.character(packageVersion("shiny")))
    )
  }
  expect_false(grepl("<a", as.character(headerTitle("shiny")), fixed = TRUE))
})

test_that("headerTitle supports an HTTP documentation URL with a port", {
  html <- as.character(headerTitle("shiny", url_docu = "http://localhost:8080"))
  expect_match(html, 'href="http://localhost:8080/news/index.html"', fixed = TRUE)
})

test_that("headerTitle validates the port range without confusing it with the path", {
  for (port in c("0", "1", "65535", "00065535")) {
    url <- paste0("https://example.org:", port, "/docu")
    html <- as.character(headerTitle("shiny", url_docu = url))
    expect_match(html, paste0('href="', url, '/news/index.html"'), fixed = TRUE)
  }

  for (port in c("65536", "99999", "4294967296", strrep("9", 400))) {
    html <- as.character(headerTitle(
      "shiny", url_docu = paste0("https://example.org:", port, "/docu")
    ))
    expect_equal(html, as.character(headerTitle("shiny")))
  }

  html <- as.character(headerTitle("shiny", url_docu = "https://example.org/docu:65536"))
  expect_match(html, 'href="https://example.org/docu:65536/news/index.html"', fixed = TRUE)
})

test_that("headerTitle uses and trims the supplied title", {
  html <- as.character(headerTitle("shiny", "  My app  "))
  expect_equal(html, as.character(headerTitle("shiny", "My app")))
  expect_match(html, "My app", fixed = TRUE)
  expect_false(grepl("shiny", html, fixed = TRUE))
})

test_that("headerTitle falls back to the package name for an empty title", {
  for (title in c("", "  ")) {
    expect_equal(
      as.character(headerTitle("shiny", title)),
      as.character(headerTitle("shiny"))
    )
  }
})

test_that("headerTitle returns the title for an uninstalled package", {
  expect_equal(
    as.character(headerTitle("notInstalledPkgXyz", "Custom title", "https://example.org/")),
    "Custom title"
  )
  expect_equal(as.character(headerTitle("notInstalledPkgXyz")), "notInstalledPkgXyz")
})

test_that("headerTitle validates pkg_name", {
  for (p in list(NULL, NA_character_, "", 1, c("shiny", "shinyTools"))) {
    expect_error(headerTitle(p), "`pkg_name` must be a single non-empty string")
  }
})

test_that("headerTitle validates title", {
  for (title in list(NULL, NA_character_, 1, c("first", "second"))) {
    expect_error(headerTitle("shiny", title), "`title` must be a single string")
  }
})
