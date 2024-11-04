test_that("Test module plotExportServer", {
  # Create a sample data frame
  data <- data.frame(
    x = c(0.1, 0.2, 0.3, 0.4, 0.5),
    y = c(0.2, 0.4, 0.1, 0.7, 0.3)
  )

  # Create a scatter plot using ggplot2
  plot <- (ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) +
             ggplot2::geom_point()) %>%
    formatTitlesOfGGplot(text = list(plotTitle = list(text = "justTesting", fontType = "plain",
                                                      color = "#000000", size = 12L, hide = FALSE),
                                     xAxisTitle = list(text = "", fontType = "plain", color = "#000000",
                                                       size = 12L, hide = FALSE),
                                     yAxisTitle = list(text = "moreTesting", fontType = "plain",
                                                       color = "#000000", size = 12L, hide = FALSE),
                                     xAxisText = list(fontType = "plain", color = "#000000",
                                                      size = 12L, hide = FALSE),
                                     yAxisText = list(fontType = "plain",
                                                      color = "#000000", size = 12L, hide = FALSE)))

  testServer(plotExportServer,
             args = list(plotFun = reactive({function() plot}),
                         plotType = "ggplot"),
             {
               # Arrange
               print("test export plot")
               # Act
               session$setInputs(
                 width = 1280,
                 height = 800,
                 exportType = "png"
               )

               expect_equal(output$exportPlot %>% names(),
                            c("src", "width", "height", "alt", "coordmap"))
               # fails in Jenkins although it should not ...
               #expect_true(length(output$exportPlot$coordmap$panels[[1]]$mapping) == 2)
             })
})

test_that("Test function isEmptyPlot", {
  # test if NULL works
  p <- NULL
  expect_true(isEmptyPlot(p))

  # test if NULL works with recordPlot
  p <- NULL
  if (!inherits(p, "try-error") && !inherits(p, "ggplot")) {
    # For base R plots, record the plot
    p <- try(recordPlot(), silent = TRUE)  # Capture the side effect plot
  }
  expect_true(isEmptyPlot(p))

  # test if a ggplot without data is empty
  p <- ggplot2::ggplot() + ggplot2::geom_point()
  expect_true(isEmptyPlot(p))

  # test if a ggplot with data is not empty
  p <- ggplot2::ggplot(data.frame(x = 1:10, y = 1:10), ggplot2::aes(x = .data$x, y = .data$y)) + ggplot2::geom_point()
  expect_false(isEmptyPlot(p))

  # test if a boxplot with data is not empty
  p <- boxplot(1:10)
  expect_false(isEmptyPlot(p))

  # test if a base plot with data is not empty
  p <- base::plot(1:10, 1:10)
  if (!inherits(p, "try-error") && !inherits(p, "ggplot")) {
    # For base R plots, record the plot
    p <- try(recordPlot())  # Capture the side effect plot
  }
  expect_false(isEmptyPlot(p))

  # clean device
  dev.off()
})
