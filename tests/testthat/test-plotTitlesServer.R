test_that("Test module plotTitlesServer", {
  testServer(plotTitlesServer,
             args = list(type = "ggplot"),
             {
               # Arrange
               print("test plot titles")
               # Act
               session$setInputs(
                 labelName = "xAxis",
                 text = "test",
                 fontType = "bold",
                 color = "#FFFFFF",
                 size = 5,
                 hide = FALSE
               )

               expect_equal(session$returned %>% reactiveValuesToList(),
                            list(plot = list(text = "", fontType = "plain", color = "#000000",
                                             size = 12L, hide = FALSE),
                                 xAxis = list(text = "test", fontType = "bold",
                                              color = "#FFFFFF", size = 5, hide = FALSE),
                                 yAxis = list(text = "", fontType = "plain", color = "#000000",
                                              size = 12L, hide = FALSE)))
             })
})

test_that("Test module formatTitlesOfGGplot", {
  # Create a sample data frame
  data <- data.frame(
    x = c(0.1, 0.2, 0.3, 0.4, 0.5),
    y = c(0.2, 0.4, 0.1, 0.7, 0.3)
  )

  # Create a scatter plot using ggplot2
  plot <- (ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) +
             ggplot2::geom_point()) %>%
    formatTitlesOfGGplot(titles = list(plot = list(text = "justTesting", fontType = "plain",
                                                   color = "#000000", size = 12L, hide = FALSE),
                                       xAxis = list(text = "", fontType = "plain", color = "#000000",
                                                    size = 12L, hide = FALSE),
                                       yAxis = list(text = "moreTesting", fontType = "plain",
                                                    color = "#000000", size = 12L, hide = FALSE)))

  # test labels and titles
  expect_equal(plot$labels, list(y = "moreTesting", title = "justTesting", x = "x"))
})
