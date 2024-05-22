test_that("Test module plotPointsServer", {
  testServer(plotPointsServer,
             args = list(type = "ggplot"),
             {
               # Arrange
               print("test plot points")
               # Act
               session$setInputs(
                 symbol = 24,
                 color = "#FF00EA",
                 colorBg = "#00FF22",
                 size = 5,
                 hide = FALSE
               )

               expect_equal(session$returned %>% reactiveValuesToList(),
                            list(dataPoints =
                                   list(symbol = 24, color = "#FF00EA", colorBg = "#00FF22",
                                        size = 5, alpha = 1, lineWidthBg = 2, hide = FALSE)))
             })
})

test_that("Test module formatPointsOfGGplot", {
  # Create a sample data frame
  data <- data.frame(
    x = c(0.1, 0.2, 0.3, 0.4, 0.5),
    y = c(0.2, 0.4, 0.1, 0.7, 0.3)
  )

  # Create a scatter plot using ggplot2
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) %>%
    formatPointsOfGGplot(pointStyle = list(dataPoints =
                                             list(symbol = 24, color = "#FF00EA", colorBg = "#00FF22",
                                                  size = 5, hide = FALSE, alpha = 0.2)))

  # test labels and titles
  expect_equal(plot$labels, list(x = "x", y = "y"))
})
