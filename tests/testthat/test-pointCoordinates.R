test_that("pointCoordinatesServer returns correct values", {
  testServer(
    pointCoordinatesServer,
    args = list(id = "test", default_name = reactive("Point 1")),
    {
      # Simulate user inputs
      session$setInputs(`label` = "Point 3")
      session$setInputs(`x-value` = 10, `x-min` = 5, `x-max` = 15)
      session$setInputs(`y-value` = 20, `y-min` = 10, `y-max` = 30)

      # Call the reactive output
      result <- session$returned()

      # Validate the returned values
      expect_equal(result$id, "Point 3")
      expect_equal(result$x, 10)
      expect_equal(result$xmin, 5)
      expect_equal(result$xmax, 15)

      expect_equal(result$y, 20)
      expect_equal(result$ymin, 10)
      expect_equal(result$ymax, 30)
    }
  )
})
