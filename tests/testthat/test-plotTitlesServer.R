test_that("Test module plotTitlesServer", {
  testServer(plotTitlesServer,
             args = list(type = "ggplot",
                         initText = NULL),
             {
               # Arrange
               print("test plot titles")
               # Act
               session$setInputs(
                 labelName = "xAxisTitle",
                 text = "test",
                 fontFamily = "mono",
                 fontType = "bold",
                 color = "#FFFFFF",
                 size = 5,
                 hide = FALSE
               )

               expect_equal(session$returned[["plotTitle"]],
                            list(text = "",  fontFamily = "sans", fontType = "plain",
                                 color = "#000000", size = 12L, hide = FALSE))
               expect_equal(session$returned[["xAxisTitle"]],
                            list(text = "test", fontFamily = "mono", fontType = "bold",
                                 color = "#FFFFFF", size = 5, hide = FALSE))

               session$setInputs(
                 labelName = "yAxisText",
                 fontFamily = "serif",
                 fontType = "bold",
                 color = "#FFFFFF",
                 size = 5,
                 hide = FALSE
               )

               expect_equal(session$returned[["yAxisText"]],
                            list(fontFamily = "serif", fontType = "bold", color = "#FFFFFF",
                                 size = 5, hide = FALSE, angle = 0, hjust = 0.5, vjust = 0.5))
             })
})

test_that("Test validateInitText", {
  testList <- list(plotTitle = list(text = "", fontFamily = "serif", fontType = "plain",
                                    color = "#000000", size = 12L, hide = FALSE),
                   xAxisTitle = list(text = "test", fontFamily = "serif", fontType = "bold",
                                     color = "#FFFFFF", size = 5, hide = FALSE),
                   yAxisText = list(fontFamily = "serif", fontType = "plain", color = "#000000",
                                    size = 10L, hide = FALSE, angle = 0L, hjust = 0.5, vjust = 0.5))

  testList <- validateInitText(testList, type = "ggplot")

  expect_equal(testList,
               list(plotTitle = list(text = "", fontFamily = "serif", fontType = "plain",
                                     color = "#000000", size = 12L, hide = FALSE),
                    xAxisTitle = list(text = "test", fontFamily = "serif", fontType = "bold",
                                      color = "#FFFFFF", size = 5, hide = FALSE),
                    xAxisText = list(fontFamily = "sans", fontType = "plain", color = "#000000", size = 10L,
                                     hide = FALSE, angle = 0L, hjust = 0.5, vjust = 0.5),
                    yAxisTitle = list(text = "", fontFamily = "sans", fontType = "plain", color = "#000000",
                                      size = 12L, hide = FALSE),
                    yAxisText = list(fontFamily = "serif", fontType = "plain", color = "#000000", size = 10L,
                                     hide = FALSE, angle = 0L, hjust = 0.5, vjust = 0.5)))
})

test_that("Test defaultInitText", {
  expect_equal(defaultInitText(type = "ggplot"),
               list(
                 plotTitle = defaultTextFormat(type = "ggplot")[["title"]],
                 xAxisTitle = defaultTextFormat(type = "ggplot")[["title"]],
                 xAxisText = defaultTextFormat(type = "ggplot")[["text"]],
                 yAxisTitle = defaultTextFormat(type = "ggplot")[["title"]],
                 yAxisText = defaultTextFormat(type = "ggplot")[["text"]]
               ))

  expect_equal(
    defaultInitText(
      type = "ggplot",
      availableElements = c("title", "axis", "legend")
    ),
    list(
      plotTitle = list(
        text = "",
        fontFamily = "sans",
        fontType = "plain",
        color = "#000000",
        size = 12L,
        hide = FALSE
      ),
      xAxisTitle = list(
        text = "",
        fontFamily = "sans",
        fontType = "plain",
        color = "#000000",
        size = 12L,
        hide = FALSE
      ),
      xAxisText = list(
        fontFamily = "sans",
        fontType = "plain",
        color = "#000000",
        size = 10L,
        hide = FALSE,
        angle = 0L,
        hjust = 0.5,
        vjust = 0.5
      ),
      yAxisTitle = list(
        text = "",
        fontFamily = "sans",
        fontType = "plain",
        color = "#000000",
        size = 12L,
        hide = FALSE
      ),
      yAxisText = list(
        fontFamily = "sans",
        fontType = "plain",
        color = "#000000",
        size = 10L,
        hide = FALSE,
        angle = 0L,
        hjust = 0.5,
        vjust = 0.5
      ),
      legendTitle = list(
        text = "",
        fontFamily = "sans",
        fontType = "plain",
        color = "#000000",
        size = 12L,
        hide = FALSE
      ),
      legendText = list(
        fontFamily = "sans",
        fontType = "plain",
        color = "#000000",
        size = 10L,
        hide = FALSE,
        angle = 0L,
        hjust = 0.5,
        vjust = 0.5
      )
    )
  )
})
