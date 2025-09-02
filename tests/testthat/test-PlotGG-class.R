test_that("PlotGG formatTitles works", {
  p <- ggplot(mtcars, aes(mpg, hp)) + geom_point()
  pg <- new_PlotGG(p)
  pg <- formatTitles(pg, list(plotTitle = list(text = "Title"), xAxisTitle = list(text = "X"), yAxisTitle = list(text = "Y")))
  plot_out <- as.ggplot(pg)
  expect_s3_class(pg, "PlotGG")
  expect_true("Title" %in% plot_out$labels$title)
  expect_true("X" %in% plot_out$labels$x)
  expect_true("Y" %in% plot_out$labels$y)
})

test_that("PlotGG formatTitles works", {
  p <- ggplot(mtcars, aes(mpg, hp)) + geom_point()
  pg <- new_PlotGG(p)
  pg <- formatTitles(pg, list(plotTitle = list(text = "Title"), xAxisTitle = list(text = "X"), yAxisTitle = list(text = "Y")))
  plot_out <- as.ggplot(pg)
  expect_s3_class(pg, "PlotGG")
  expect_true("Title" %in% plot_out$labels$title)
  expect_true("X" %in% plot_out$labels$x)
  expect_true("Y" %in% plot_out$labels$y)
})

test_that("PlotGG formatScales works", {
  p <- ggplot(mtcars, aes(mpg, hp)) + geom_point()
  pg <- new_PlotGG(p)
  pg <- formatScales(pg, list(
    xAxis = list(fromData = FALSE, min = 10, max = 35),
    yAxis = list(fromData = FALSE, min = 50, max = 150)
  ))
  expect_s3_class(pg, "PlotGG")
})

test_that("PlotGG addCustomPoints works", {
  p <- ggplot(mtcars, aes(x = .data$mpg, y = .data$hp)) + geom_line()
  pg <- new_PlotGG(p)
  #points_df <- data.frame(x = 25, y = 100, color = "red")

  points_list <- test_custom_points <- list(
    `Point 1` = list(id = "Point 1", x = 4L, y = 6L,
                     xmin = 2, xmax = 4, ymin = as.numeric(NA), ymax = as.numeric(NA),
                     point_symbol = 19L, point_color = "#000000",
                     point_colorBg = "#000000", point_size = 10L, point_alpha = 1L,
                     point_lineWidthBg = 2L, point_hide = FALSE, label_text = "",
                     error_linetype = 1L, error_color = "#000000", error_alpha = 1L,
                     error_size = 1L, error_width = 1L, error_hide = FALSE,
                     label_useExpression = FALSE, label_expression = "", label_fontFamily = "sans",
                     label_fontType = "bold", label_color = "#F2AA5E", label_size = 16L,
                     label_hide = FALSE, label_angle = 90L, label_hjust = 0.5,
                     label_vjust = 0.5),
    `Point 2` = list(id = "Point 2", x = 5L, y = -4L,
                     xmin = 2, xmax = 9L, ymin = -10L, ymax = 0L,
                     point_symbol = 15,
                     point_color = "#2EE854", point_colorBg = "#000000", point_size = 9L,
                     point_alpha = 1L, point_lineWidthBg = 2L, point_hide = FALSE,
                     error_linetype = 2L, error_color = "#2EE854", error_alpha = 1L,
                     error_size = 1L, error_width = 1L, error_hide = FALSE,
                     label_text = "", label_useExpression = TRUE, label_expression = "sigma^~C+3~x",
                     label_fontFamily = "sans", label_fontType = "plain", label_color = "#000000",
                     label_size = 5L, label_hide = FALSE, label_angle = 0L, label_hjust = 1,
                     label_vjust = 0.5),
    `Point 3` = list(id = "Point 3", x = 8L, y = 4L,
                     xmin = 6, xmax = 10L, ymin = 2L, ymax = 6,
                     point_symbol = 19L,
                     point_color = "#000000", point_colorBg = "#000000", point_size = 1L,
                     point_alpha = 1L, point_lineWidthBg = 2L, point_hide = FALSE,
                     error_linetype = 3L, error_color = "#F2AA5E", error_alpha = 1L,
                     error_size = 1L, error_width = 1L, error_hide = FALSE,
                     label_text = "p3", label_useExpression = FALSE, label_expression = "",
                     label_fontFamily = "sans", label_fontType = "bold", label_color = "#F2AA5E",
                     label_size = 16L, label_hide = FALSE, label_angle = 180L, label_hjust = -0,
                     label_vjust = -1)
  )

  points_df <- lapply(points_list, as.data.frame) %>%
    bind_rows()
  pg <- addCustomPoints(pg, points_list)
  expect_s3_class(pg, "PlotGG")
})

test_that("PlotGG formatLegend works", {
  p <- ggplot(mtcars, aes(mpg, hp, color = factor(cyl))) + geom_point()
  pg <- new_PlotGG(p)
  dummy_legend <- list(
    position = "right",
    direction = "vertical",
    layout = list(
      title = list(list(text = "Legend Title")),
      labels = list(`4` = list(text = "Four"), `6` = list(text = "Six"), `8` = list(text = "Eight"))
    )
  )
  pg <- formatLegend(pg, dummy_legend)
  expect_s3_class(pg, "PlotGG")
})
