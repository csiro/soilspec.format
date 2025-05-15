# Hone Lab Red format unit tests

source("common_test.R", local = T)

test_that("Get Hone Lab Red example file path", {
  expected <- system.file("extdata", "HoneLabRed",
                          "example.hlr", package = "soilspec.format")

  actual <- soilspec.format::hone.lab.red.file.path()

  testthat::expect_equal(object = actual, expected = expected)
})

test_that("Read Hone Lab Red example file", {
  suppressWarnings({
    result <- common_test(soil.format.obj = soilspec.format::HoneLabRed$new(),
                          test_file_path = soilspec.format::hone.lab.red.file.path(),
                          status = 0, mode = NULL,
                          is.absorbance = F, is.reflectance = F, is.transmittance = F,
                          is.descending = F, num.data.rows = 513,
                          wavenumbers = c(1350.0, 2550.0),
                          intensities = c(0.10395035, 0.09771062),
                          metadata.length = 39)
  })
})

test_that("Read non-existent Hone Lab Red file", {
  hone.lab.red <- soilspec.format::HoneLabRed$new()

  path <- "nothing at all"
  result <- hone.lab.red$read(path)

  testthat::expect_false(result$status == 0)
  testthat::expect_null(result$spec.df)
  testthat::expect_null(result$meta.df)
  testthat::expect_null(result$mode)
})
