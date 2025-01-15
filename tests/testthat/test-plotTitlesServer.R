test_that("Test module plotTitlesServer for a title", {
  testServer(plotTitlesServer, args = list(type = "ggplot", initText = NULL), {
    # Arrange
    print("test plot titles for titles")
    # Act
    session$setInputs(
      labelName = "xAxisTitle"
    )
    session$setInputs(
      `text-text` = "test",
      `text-useExpression` = FALSE,
      `text-expression` = "",
      `text-fontFamily` = "mono",
      `text-fontType` = "bold",
      `text-color` = "#FFFFFF",
      `text-size` = 5,
      `text-hide` = FALSE
    )

    expect_equal(
      session$returned[["plotTitle"]],
      list(
        text = "",
        useExpression = FALSE,
        expression = "",
        fontFamily = "sans",
        fontType = "plain",
        color = "#000000",
        size = 12L,
        hide = FALSE
      )
    )
    expect_equal(
      session$returned[["xAxisTitle"]],
      list(
        text = "test",
        useExpression = FALSE,
        expression = "",
        fontFamily = "mono",
        fontType = "bold",
        color = "#FFFFFF",
        size = 5,
        hide = FALSE
      )
    )
  })
})

test_that("Test module plotTitlesServer for axis text", {
  testServer(plotTitlesServer, args = list(type = "ggplot", initText = NULL), {
    # Arrange
    print("test plot titles for axis text")
    # Act
    session$setInputs(
      labelName = "yAxisText"
    )

    session$setInputs(
      `text-fontFamily` = "serif",
      `text-fontType` = "bold",
      `text-color` = "#FFFFFF",
      `text-size` = 5,
      `text-hide` = FALSE,
      `text-angle` = 0,
      `text-hjust` = 0.5,
      `text-vjust` = 0.5
    )

    expect_equal(
      session$returned[["yAxisText"]],
      list(
        fontFamily = "serif",
        fontType = "bold",
        color = "#FFFFFF",
        size = 5,
        hide = FALSE,
        angle = 0,
        hjust = 0.5,
        vjust = 0.5
      )
    )
  })
})

test_that("Test defaultInitText", {
  expect_equal(
    defaultInitText(type = "ggplot"),
    list(
      plotTitle = defaultTextFormat(type = "ggplot")[["title"]],
      xAxisTitle = defaultTextFormat(type = "ggplot")[["title"]],
      xAxisText = defaultTextFormat(type = "ggplot")[["text"]],
      yAxisTitle = defaultTextFormat(type = "ggplot")[["title"]],
      yAxisText = defaultTextFormat(type = "ggplot")[["text"]]
    )
  )

  expect_equal(
    defaultInitText(
      type = "ggplot",
      availableElements = c("title", "axis", "legend")
    ),
    list(
      plotTitle = list(
        text = "",
        useExpression = FALSE,
        expression = "",
        fontFamily = "sans",
        fontType = "plain",
        color = "#000000",
        size = 12L,
        hide = FALSE
      ),
      xAxisTitle = list(
        text = "",
        useExpression = FALSE,
        expression = "",
        fontFamily = "sans",
        fontType = "plain",
        color = "#000000",
        size = 12L,
        hide = FALSE
      ),
      xAxisText = list(
        text = "",
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
        useExpression = FALSE,
        expression = "",
        fontFamily = "sans",
        fontType = "plain",
        color = "#000000",
        size = 12L,
        hide = FALSE
      ),
      yAxisText = list(
        text = "",
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
        useExpression = FALSE,
        expression = "",
        fontFamily = "sans",
        fontType = "plain",
        color = "#000000",
        size = 12L,
        hide = FALSE
      ),
      legendText = list(
        text = "",
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
