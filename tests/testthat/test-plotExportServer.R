test_that("Test module plotExportServer", {
  # Create a sample data frame
  data <- data.frame(
    x = c(1, 2, 3, 4, 5),
    y = c(2, 4, 1, 7, 3)
  )

  # Create a scatter plot using ggplot2
  plot <-   (ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point()) %>%
    formatTitlesOfGGPlot(plotTitle = defaultTitleFormat(text = "justTesting"),
                         axisTitleX = defaultTitleFormat(),
                         axisTitleY = defaultTitleFormat(text = "moreTesting"))

  testServer(plotExportServer,
             args = list(plotFun = reactive({function() plot})),
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