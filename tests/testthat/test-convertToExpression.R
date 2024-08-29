test_that("Test convertToExpression", {
  expect_equal(convertToExpression('"test text" ~ with[subscript]'),
               expression("test text" ~ with[subscript]))
  expect_equal(convertToExpression('"test text." ~ With^superscript ~ C'),
               expression("test text." ~ With^superscript ~ C))
  expect_equal(convertToExpression(""), "")
  expect_equal(convertToExpression('"special character" ~ gamma*delta ~ "%"'),
               expression("special character" ~ gamma * delta ~ "%"))
})
