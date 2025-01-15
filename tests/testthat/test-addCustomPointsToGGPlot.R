test_that("addCustomPointsToGGplot adds points to a ggplot", {
  # Create a base ggplot object
  base_plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) +
    ggplot2::geom_line()

  # Define a dataset for points
  points_data <- data.frame(
    wt = c(2.5, 3.5, 4.5),
    mpg = c(15, 20, 25)
  )

  # Call the function to add points
  updated_plot <- addCustomPointsToGGplot(
    plot = base_plot,
    data = points_data
  )

  # Test if the updated plot has the expected layers
  expect_equal(length(updated_plot$layers), 2) # One line layer + one point layer

  # Test if the points layer is added correctly
  expect_true("GeomPoint" %in% class(updated_plot$layers[[2]]$geom))

  # Test if the data in the points layer matches the input data
  expect_equal(updated_plot$layers[[2]]$data, points_data)
})
