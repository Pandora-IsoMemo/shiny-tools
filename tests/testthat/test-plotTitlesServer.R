test_that("Test module plotTitlesServer", {
  testServer(plotTitlesServer,
             {
               # Arrange
               print("test plot titles")
               # Act
               session$setInputs(
                 labelName = "xAxis",
                 text = "test",
                 fontType = 2,
                 color = "#FFFFFF",
                 size = 5,
                 hide = FALSE
               )

               expect_equal(session$returned(),
                            list(xAxis = list(text = "test", fontType = 2, color = "#FFFFFF",
                                                   size = 5, hide = FALSE)))
             })
})
