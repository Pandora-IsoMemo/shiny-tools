test_that("Test module vectorInputServer", {
  testServer(vectorInputServer,
             args = list(defaultInputs = reactive(c(a = 1, b = 2))),
             {
               # Arrange
               print("test vector input")
               # Act
               session$setInputs(
                 index_select = 2,
                 new_value = 5
               )

               expect_equal(session$returned(), c(a = 1, b = 2))

               session$setInputs(
                 submit_btn = 1
               )

               expect_equal(session$returned(), c(a = 1, b = 5))
             })
})
