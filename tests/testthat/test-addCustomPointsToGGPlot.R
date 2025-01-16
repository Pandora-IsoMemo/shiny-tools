test_that("addCustomPointsToGGplot adds points to a ggplot", {
  # Create a base ggplot object
  base_plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) +
    ggplot2::geom_line()

  test_custom_points <- list(
    `Point 1` = list(
      id = "Point 1",
      x = 3L,
      y = 6L,
      xmin = NA,
      xmax = NA,
      ymin = NA,
      ymax = NA,
      text = "",
      useExpression = FALSE,
      expression = "",
      fontFamily = "sans",
      fontType = "plain",
      color = "#000000",
      size = 12L,
      hide = FALSE,
      angle = 0L,
      hjust = 0.5,
      vjust = 0.5
    ),
    `Point 2` = list(
      id = "Point 2",
      x = 5L,
      y = 8L,
      xmin = 1L,
      xmax = 7L,
      ymin = NA,
      ymax = 10L,
      text = "ydghx",
      useExpression = FALSE,
      expression = "",
      fontFamily = "sans",
      fontType = "plain",
      color = "#23E843",
      size = 18L,
      hide = FALSE,
      angle = 0L,
      hjust = 0.5,
      vjust = 0.5
    )
  )

  # Call the function to add points
  updated_plot <- addCustomPointsToGGplot(plot = base_plot, custom_points = test_custom_points)

  # Test if the updated plot has the expected layers
  expect_equal(length(updated_plot$layers), 2) # One line layer + one point layer

  # Test if the points layer is added correctly
  expect_true("GeomPoint" %in% class(updated_plot$layers[[2]]$geom))

  # Test if the data in the points layer matches the input data
  expect_equal(updated_plot$layers[[2]]$data,
               test_custom_points %>% lapply(FUN = as.data.frame) %>% dplyr::bind_rows())
})
