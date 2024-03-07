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
                 max = 20
               )

               expect_equal(session$returned %>% reactiveValuesToList(),
                            list(xAxis = list(min = 10, max = 20),
                                 yAxis = list(min = 0L, max = 1L)))
             })
})
