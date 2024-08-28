test_that("Test convertToExpression", {
  expect_equal(convertToExpression("test text with[subscript]"),
               expression("test text" ~ with[subscript]))
  expect_equal(convertToExpression("test text. With^superscript"),
               expression("test text." ~ With^superscript))
  expect_equal(convertToExpression(""), "")
  expect_equal(convertToExpression("not finished^"), expression("not finished^"))
  expect_equal(convertToExpression("open["), expression("open["))
  expect_equal(convertToExpression("open("), expression("open("))
  expect_equal(convertToExpression("is closed()"), expression("is closed()"))
  expect_equal(convertToExpression("is closed() here"), expression("is closed() here"))
  expect_equal(convertToExpression("special character:"), expression("special character:"))
  expect_equal(convertToExpression("special character %"), expression("special character %"))
})
