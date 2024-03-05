test_that("Test module dataExportServer", {
  testServer(dataExportServer,
             args = list(dataFun = reactive({function() mtcars})),
             {
               # Arrange
               print("test export 'mtcars'")
               # Act
               session$setInputs(
                 export = 1,
                 exportType = "xlsx"
               )

               expect_equal(sapply(dataFun()(), class),
                            c(mpg = "numeric", cyl = "numeric", disp = "numeric", hp = "numeric",
                              drat = "numeric", wt = "numeric", qsec = "numeric", vs = "numeric",
                              am = "numeric", gear = "numeric", carb = "numeric"))
             })
})
