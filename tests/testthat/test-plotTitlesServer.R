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

test_that("Test module plotTitlesServer", {
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