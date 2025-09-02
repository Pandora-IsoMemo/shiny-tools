# Helper used in tests (uses the package's config()) -----------------------
testPlotFun <- function(plot_type) {
  switch(
    plot_type,
    "line" = ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) + ggplot2::geom_line(),
    "box" = ggplot2::ggplot(mtcars, ggplot2::aes(x = factor(cyl), y = mpg)) + ggplot2::geom_boxplot(),
    "density" = ggplot2::ggplot(mtcars, ggplot2::aes(x = cyl)) + ggplot2::geom_density(),
    "histogram" = ggplot2::ggplot(mtcars, ggplot2::aes(x = cyl, fill = factor(am))) +
      ggplot2::geom_histogram(alpha = 0.5, binwidth = NULL, position = "identity")
  )
}

make_custom_point <- function(
    id, x, y,
    xmin = NA_real_, xmax = NA_real_,
    ymin = NA_real_, ymax = NA_real_,

    point_symbol  = config()$defaultPointStyle$dataPoints$symbol,
    point_size    = config()$defaultPointStyle$dataPoints$size,
    point_color   = config()$defaultPointStyle$dataPoints$color,
    point_colorBg = config()$defaultPointStyle$dataPoints$colorBg,
    point_alpha   = config()$defaultPointStyle$dataPoints$alpha,
    point_hide    = config()$defaultPointStyle$dataPoints$hide,

    label_text          = id,
    label_useExpression = FALSE,
    label_expression    = "",
    label_fontFamily    = config()$defaultGGLabel$fontFamily,
    label_fontType      = config()$defaultGGLabel$fontType,
    label_color         = config()$defaultGGLabel$color,
    label_size          = config()$defaultGGLabel$size,
    label_hide          = config()$defaultGGLabel$hide,
    label_angle         = config()$defaultGGLabel$angle,
    label_hjust         = config()$defaultGGLabel$hjust,
    label_vjust         = config()$defaultGGLabel$vjust
) {
  list(
    id = id,
    x = x, y = y,
    xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
    point_symbol = point_symbol,
    point_size   = point_size,
    point_color  = point_color,
    point_colorBg= point_colorBg,
    point_alpha  = point_alpha,
    point_hide   = point_hide,
    label_text = label_text,
    label_useExpression = label_useExpression,
    label_expression = label_expression,
    label_fontFamily = label_fontFamily,
    label_fontType = label_fontType,
    label_color = label_color,
    label_size = label_size,
    label_hide = label_hide,
    label_angle = label_angle,
    label_hjust = label_hjust,
    label_vjust = label_vjust
  )
}

# Example custom_points lists (use YAML-driven defaults) ----------------
custom_points_line <- list(
  make_custom_point(
    id = "P1", x = 3.0, y = 22,
    xmin = 2.8, xmax = 3.2, ymin = 20, ymax = 24,
    point_color = "#1f77b4", point_size = 3.5
  ),
  make_custom_point(
    id = "P2", x = 4.5, y = 18,
    ymin = 17, ymax = 19,
    point_color = "#d62728", point_size = 3.5,
    label_text = "Low here", label_hjust = -0.1, label_vjust = -0.6
  )
)

custom_points_box <- list(
  make_custom_point(
    id = "B-4", x = "4", y = 26,
    ymin = 24, ymax = 28,
    point_color = "#2ca02c", point_size = 3.5
  ),
  make_custom_point(
    id = "B-8", x = "8", y = 15,
    point_color = "#1f77b4", label_text = "Eight!", label_hjust = 1, label_vjust = -0.5
  )
)

custom_points_density <- list(
  make_custom_point(
    id = "D1", x = 6, y = 0.08,
    ymin = 0.06, ymax = 0.10,
    point_color = "#9467bd", point_size = 3.5
  ),
  make_custom_point(
    id = "D2", x = 4.5, y = 0.05,
    point_color = "#d62728", label_text = "Peak-ish", label_hjust = -0.2, label_vjust = -0.6
  )
)

custom_points_hist <- list(
  make_custom_point(
    id = "H1", x = 6, y = 4,
    ymin = 3, ymax = 5,
    point_color = "#2ca02c", point_size = 3.5
  ),
  make_custom_point(
    id = "H2", x = 7, y = 3,
    point_color = "#9467bd", label_text = "Between bins", label_hjust = 0.8, label_vjust = 1.2
  )
)

# Tests per plot type ----------------------------------

testthat::test_that("addCustomPointsToGGplot_internal: line plot builds and adds layers", {
  base <- testPlotFun("line")
  testthat::expect_s3_class(base, "ggplot")
  base_layers <- length(base$layers)

  p <- base %>% addCustomPointsToGGplot_internal(custom_points_line)
  testthat::expect_s3_class(p, "ggplot")

  # should build without error
  testthat::expect_error(ggplot2::ggplot_build(p), NA)

  # should add layers beyond the base
  testthat::expect_gt(length(p$layers), base_layers)
})

testthat::test_that("addCustomPointsToGGplot_internal: boxplot builds (factor x) and adds layers", {
  base <- testPlotFun("box")
  testthat::expect_s3_class(base, "ggplot")
  base_layers <- length(base$layers)

  p <- base %>% addCustomPointsToGGplot_internal(custom_points_box)
  testthat::expect_s3_class(p, "ggplot")
  testthat::expect_error(ggplot2::ggplot_build(p), NA)
  testthat::expect_gt(length(p$layers), base_layers)
})

testthat::test_that("addCustomPointsToGGplot_internal: density plot builds and adds layers", {
  base <- testPlotFun("density")
  testthat::expect_s3_class(base, "ggplot")
  base_layers <- length(base$layers)

  p <- base %>% addCustomPointsToGGplot_internal(custom_points_density)
  testthat::expect_s3_class(p, "ggplot")
  testthat::expect_error(ggplot2::ggplot_build(p), NA)
  testthat::expect_gt(length(p$layers), base_layers)
})

testthat::test_that("addCustomPointsToGGplot_internal: histogram builds (no inherited fill issues) and adds layers", {
  base <- testPlotFun("histogram")
  testthat::expect_s3_class(base, "ggplot")
  base_layers <- length(base$layers)

  p <- base %>% addCustomPointsToGGplot_internal(custom_points_hist)
  testthat::expect_s3_class(p, "ggplot")

  # Build should not error (previously failed due to inherited fill = factor(am))
  testthat::expect_error(ggplot2::ggplot_build(p), NA)
  testthat::expect_gt(length(p$layers), base_layers)

  # Optional: check we really have errorbar layers present (h or v) for these points
  geoms <- vapply(p$layers, function(L) class(L$geom)[1], character(1))
  testthat::expect_true(any(grepl("GeomErrorbar", geoms)) || any(grepl("GeomErrorbarh", geoms)))
})

# Additional tests for addCustomPointsToGGplot_internal ---------------

test_that("addCustomPointsToGGplot_internal adds points to a ggplot", {
  # Create a base ggplot object
  base_plot <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_line()

  test_custom_points <- list(
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

  # Call the function to add points
  updated_plot <- base_plot %>% addCustomPointsToGGplot_internal(custom_points = test_custom_points)

  # Test if the updated plot has the expected layers
  expect_equal(length(updated_plot$layers), 6)

  # Test if the points layer is added correctly
  expect_true(all(
    c("GeomPoint", "GeomLine", "GeomText", "GeomErrorbar", "GeomErrorbarh") %in%
      unlist(sapply(1:6, function(layer) class(updated_plot$layers[[layer]]$geom)))
    ))

  # Test if the data in the points layer matches the input data
  expect_equal(updated_plot$layers[[2]]$data,
               test_custom_points %>% lapply(FUN = as.data.frame) %>% bind_rows())
})
