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

test_that("Test exportXLSX", {
  cars <- c("car1", "car2", "car3")
  names(cars) <- cars
  dataList <- lapply(cars, function(x) mtcars )

  # test not available yet
  # expect_no_error(exportXLSX(file = file.path(tempdir(), "test_file.xlsx"),
  #                            dat = dataList[[1]]))
  # expect_no_error(exportXLSX(file = file.path(tempdir(), "test_file.xlsx"),
  #                            dat = dataList))
  expect_error(exportXLSX(file = file.path(tempdir(), "test_file.xlsx"),
                          dat = list()))
})

test_that("Test exportJSON", {
  cars <- c("car1", "car2", "car3")
  names(cars) <- cars
  dataList <- lapply(cars, function(x) mtcars )

  # test not available yet
  # expect_no_error(exportJSON(file = file.path(tempdir(), "test_file.json"),
  #                            dat = dataList[[1]]))
  # expect_no_error(exportJSON(file = file.path(tempdir(), "test_file.json"),
  #                            dat = dataList))
  expect_error(exportJSON(file = file.path(tempdir(), "test_file.json"),
                          dat = list(a = list())))
})

test_that("Test exportCSV", {
  cars <- c("car1", "car2", "car3")
  names(cars) <- cars
  dataList <- lapply(cars, function(x) mtcars )

  # test not available yet
  # expect_no_error(exportCSV(file = file.path(tempdir(), "test_file.csv"),
  #                           dat = dataList[[1]],
  #                           colseparator = ", ",
  #                           decseparator = "."))
  # expect_no_error(exportCSV(file = file.path(tempdir(), "test_file.csv"),
  #                           dat = dataList,
  #                           colseparator = ", ",
  #                           decseparator = "."))
  expect_error(exportCSV(file = file.path(tempdir(), "test_file.csv"),
                         dat = list(a = list()),
                         colseparator = ", ",
                         decseparator = "."))
})
