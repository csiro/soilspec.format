# ASD SCO binary unit tests

source("common_test.R", local = T)

test_that("Get ASD SCO binary example file path", {
  expected <- system.file("extdata", "ASDScoBinary",
                          "example.asd.sco", package = "soilspec.format")

  actual <- soilspec.format::asd.sco.binary.file.path()

  testthat::expect_equal(object = actual, expected = expected)
})

test_that("Read ASD SCO binary example file", {
  suppressWarnings({
    result <- common_test(soil.format.obj = soilspec.format::ASDScoBinary$new(),
                          test_file_path = soilspec.format::asd.sco.binary.file.path(),
                          status = 0, mode = NULL,
                          is.absorbance = F, is.reflectance = F, is.transmittance = F,
                          is.descending = F, num.data.rows = 2151,
                          wavenumbers = c(350, 2500),
                          intensities = c(0.1168832, 0.5238607),
                          metadata.length = 1)
  })
})

test_that("Read non-existent ASD SCO binary file", {
  asd.sco.binary <- soilspec.format::ASDScoBinary$new()

  path <- "nothing at all"
  result <- asd.sco.binary$read(path)

  testthat::expect_false(result$status == 0)
  testthat::expect_null(result$spec.df)
  testthat::expect_null(result$meta.df)
  testthat::expect_null(result$mode)
})
