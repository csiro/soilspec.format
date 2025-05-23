# ASD SCO binary format unit tests

source("common_test.R", local = T)

soil.format.obj <- soilspec.format::ASDScoBinary$new()
test.file.path <- soilspec.format::asd.sco.binary.file.path()

test_that("Get ASD SCO binary example file path", {
  expected <- system.file("extdata", "ASDScoBinary",
                          "example.asd.sco", package = "soilspec.format")

  actual <- test.file.path

  testthat::expect_equal(object = actual, expected = expected)
})

test_that("Read ASD SCO binary example file", {
  suppressWarnings({
    result <- common_test(soil.format.obj = soil.format.obj,
                          test.file.path = test.file.path,
                          status = 0, mode = NULL,
                          is.absorbance = F, is.reflectance = F, is.transmittance = F,
                          is.descending = F, num.data.rows = 2151,
                          wavenumbers = c(350, 2500),
                          intensities = c(0.1168832, 0.5238607),
                          metadata.length = 1)
  })
})

test_that("Read non-existent ASD SCO binary file", {
  path <- "nothing at all"
  result <- soil.format.obj$read(path)

  testthat::expect_false(result$status == 0)
  testthat::expect_null(result$spec.df)
  testthat::expect_null(result$meta.df)
  testthat::expect_null(result$mode)
})
