test_that("Test module plotTitlesServer", {
  testServer(plotTitlesServer,
             args = list(type = "ggplot"),
             {
               # Arrange
               print("test plot titles")
               # Act
               session$setInputs(
                 labelName = "xAxisTitle",
                 text = "test",
                 fontType = "bold",
                 color = "#FFFFFF",
                 size = 5,
                 hide = FALSE
               )

               expect_equal(session$returned %>% reactiveValuesToList(),
                            list(plotTitle = list(text = "", fontType = "plain", color = "#000000",
                                                  size = 12L, hide = FALSE),
                                 xAxisTitle = list(text = "test", fontType = "bold",
                                                   color = "#FFFFFF", size = 5, hide = FALSE),
                                 yAxisTitle = list(text = "", fontType = "plain", color = "#000000",
                                                   size = 12L, hide = FALSE),
                                 xAxisText = list(fontType = "plain", color = "#000000", size = 10L,
                                                  hide = FALSE),
                                 yAxisText = list(fontType = "plain", color = "#000000", size = 10L,
                                                  hide = FALSE))
                            )
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
    formatTitlesOfGGplot(text = list(plotTitle = list(text = "justTesting", fontType = "plain",
                                                      color = "#000000", size = 12L, hide = FALSE),
                                     xAxisTitle = list(text = "", fontType = "plain", color = "#000000",
                                                       size = 12L, hide = FALSE),
                                     yAxisTitle = list(text = "moreTesting", fontType = "plain",
                                                      color = "#000000", size = 12L, hide = FALSE),
                                     xAxisText = list(fontType = "plain", color = "#000000", size = 10L,
                                                      hide = FALSE),
                                     yAxisText = list(fontType = "plain", color = "#000000", size = 10L,
                                                      hide = FALSE)
                                     ))

  # test labels and titles
  expect_equal(plot$labels, list(y = "moreTesting", title = "justTesting", x = "x"))
})

test_that("Test validateInitText", {
  testList <- list(plotTitle = list(text = "", fontType = "plain", color = "#000000",
                                    size = 12L, hide = FALSE),
                   xAxisTitle = list(text = "test", fontType = "bold",
                                     color = "#FFFFFF", size = 5, hide = FALSE),
                   yAxisText = list(fontType = "plain", color = "#000000", size = 10L,
                                    hide = FALSE))

  testList <- validateInitText(testList, type = "ggplot")

  expect_equal(testList,
               list(plotTitle = list(text = "", fontType = "plain", color = "#000000",
                                     size = 12L, hide = FALSE),
                    xAxisTitle = list(text = "test", fontType = "bold",
                                      color = "#FFFFFF", size = 5, hide = FALSE),
                    yAxisTitle = list(text = "", fontType = "plain", color = "#000000",
                                      size = 12L, hide = FALSE),
                    xAxisText = list(fontType = "plain", color = "#000000", size = 10L,
                                     hide = FALSE),
                    yAxisText = list(fontType = "plain", color = "#000000", size = 10L,
                                     hide = FALSE)))
})
