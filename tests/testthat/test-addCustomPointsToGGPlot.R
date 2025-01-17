test_that("addCustomPointsToGGplot adds points to a ggplot", {
  # Create a base ggplot object
  base_plot <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_line()

  test_custom_points <- list(
    `Point 1` = list(id = "Point 1", x = 4L, y = 6L, xmin = NA,
                     xmax = NA, ymin = NA, ymax = NA, point_symbol = 19L, point_color = "#000000",
                     point_colorBg = "#000000", point_size = 10L, point_alpha = 1L,
                     point_lineWidthBg = 2L, point_hide = FALSE, label_text = "",
                     label_useExpression = FALSE, label_expression = "", label_fontFamily = "sans",
                     label_fontType = "bold", label_color = "#F2AA5E", label_size = 16L,
                     label_hide = FALSE, label_angle = 90L, label_hjust = 0.5,
                     label_vjust = 0.5),
    `Point 2` = list(id = "Point 2", x = 5L,
                     y = -4L, xmin = NA, xmax = 9L, ymin = -10L, ymax = 0L, point_symbol = 15,
                     point_color = "#2EE854", point_colorBg = "#000000", point_size = 9L,
                     point_alpha = 1L, point_lineWidthBg = 2L, point_hide = FALSE,
                     label_text = "", label_useExpression = TRUE, label_expression = "sigma^~C+3~x",
                     label_fontFamily = "sans", label_fontType = "plain", label_color = "#000000",
                     label_size = 5L, label_hide = FALSE, label_angle = 0L, label_hjust = 1,
                     label_vjust = 0.5),
    `Point 3` = list(id = "Point 3", x = 8L,
                     y = 4L, xmin = NA, xmax = NA, ymin = NA, ymax = NA, point_symbol = 19L,
                     point_color = "#000000", point_colorBg = "#000000", point_size = 1L,
                     point_alpha = 1L, point_lineWidthBg = 2L, point_hide = FALSE,
                     label_text = "", label_useExpression = FALSE, label_expression = "",
                     label_fontFamily = "sans", label_fontType = "bold", label_color = "#F2AA5E",
                     label_size = 16L, label_hide = FALSE, label_angle = 180L, label_hjust = -0,
                     label_vjust = -1)
    )

  # Call the function to add points
  updated_plot <- base_plot %>% addCustomPointsToGGplot(custom_points = test_custom_points)

  # Test if the updated plot has the expected layers
  expect_equal(length(updated_plot$layers), 3) # One line layer + one point layer + one text layer

  # Test if the points layer is added correctly
  expect_true("GeomPoint" %in% class(updated_plot$layers[[2]]$geom))

  # Test if the data in the points layer matches the input data
  expect_equal(updated_plot$layers[[2]]$data,
               test_custom_points %>% lapply(FUN = as.data.frame) %>% bind_rows())
})
