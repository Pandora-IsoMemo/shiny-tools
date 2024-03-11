test_that("Test module plotExportServer", {
  # Create a sample data frame
  data <- data.frame(
    x = c(0.1, 0.2, 0.3, 0.4, 0.5),
    y = c(0.2, 0.4, 0.1, 0.7, 0.3)
  )

  # Create a scatter plot using ggplot2
  plot <- (ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) +
             ggplot2::geom_point()) %>%
    formatTitlesOfGGPlot(titles = list(plot = list(text = "justTesting", fontType = "plain",
                                                   color = "#000000", size = 12L, hide = FALSE),
                                       xAxis = list(text = "", fontType = "plain", color = "#000000",
                                                    size = 12L, hide = FALSE),
                                       yAxis = list(text = "moreTesting", fontType = "plain",
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

               expect_equal(output$plot %>% names(),
                            c("src", "width", "height", "alt", "coordmap"))
               expect_true(length(output$plot$coordmap$panels[[1]]$mapping) == 2)
             })
})
