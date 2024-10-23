test_that("resolveValue works with reactive variables", {
  reactive_var <- reactive({
    "This is a reactive value"
  })

  # Use testServer to create a reactive context for testing
  testServer(
    app = function(input, output, session) {
    },
    # Minimal Shiny server function
    expr = {
      expect_equal(resolveValue(reactive_var), "This is a reactive value")
    }
  )
})

test_that("resolveValue works with non-reactive variables", {
  non_reactive_var <- "This is a simple character"
  expect_equal(resolveValue(non_reactive_var), "This is a simple character")
})

test_that("resolveValue works with numeric values", {
  non_reactive_numeric <- 42
  reactive_numeric <- reactive({
    42
  })

  expect_equal(resolveValue(non_reactive_numeric), 42)

  testServer(
    app = function(input, output, session) {
    },
    # Minimal Shiny server function
    expr = {
      expect_equal(resolveValue(reactive_numeric), 42)
    }
  )
})

test_that("resolveValue works with complex data types", {
  list_var <- list(a = 1, b = 2)
  reactive_list <- reactive({
    list(a = 1, b = 2)
  })

  expect_equal(resolveValue(list_var), list(a = 1, b = 2))

  testServer(
    app = function(input, output, session) {
    },
    # Minimal Shiny server function
    expr = {
      expect_equal(resolveValue(reactive_list), list(a = 1, b = 2))
    }
  )
})
