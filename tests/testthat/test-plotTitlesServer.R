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
