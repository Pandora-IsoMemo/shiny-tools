test_that("Test module plotRangesServer", {
  testServer(plotRangesServer,
             args = list(type = "ggplot"),
             {
               # Arrange
               print("test plot ranges")
               # Act
               session$setInputs(
                 labelName = "xAxis",
                 min = 10,
                 max = 20,
                 fromData = FALSE
               )

               expect_equal(session$returned %>% reactiveValuesToList(),
                            list(xAxis = list(min = 10, max = 20, fromData = FALSE),
                                 yAxis = list(min = 0L, max = 1L, fromData = TRUE)))

               session$setInputs(
                 labelName = "xAxis",
                 min = 10,
                 max = 20,
                 fromData = TRUE
               )

               expect_equal(session$returned %>% reactiveValuesToList(),
                            list(xAxis = list(min = 10, max = 20, fromData = TRUE),
                                 yAxis = list(min = 0L, max = 1L, fromData = TRUE)))
             })
})

test_that("Test module formatRangesOfGGplot", {
  # Create a sample data frame
  data <- data.frame(
    x = c(0.1, 0.2, 0.3, 0.4, 0.5),
    y = c(0.2, 0.4, 0.1, 0.7, 0.3)
  )

  # Create a scatter plot using ggplot2
  plot <- (ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) +
             ggplot2::geom_point()) %>%
    formatRangesOfGGplot(ranges = list(xAxis = list(min = -1, max = 2, fromData = FALSE),
                                       yAxis = list(min = 0.3, max = 0.5, fromData = TRUE)))

  # test labels and titles
  expect_equal(plot$labels, list(x = "x", y = "y"))
})
