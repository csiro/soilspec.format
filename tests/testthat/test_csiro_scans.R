# CSIRO SCANS format unit tests

source("common_test.R", local = T)

soil.format.obj <- soilspec.format::SCANS$new()
test.file.path <- soilspec.format::csiro.scans.file.path()

test_that("Get CSIRO SCANS example file path", {
  expected <- system.file("extdata", "CSIROSCANS",
                          "example.scan", package = "soilspec.format")

  actual <- test.file.path

  testthat::expect_equal(object = actual, expected = expected)
})

test_that("Read CSIRO SCANS example file", {
  result <- common_soil_format_object_test(soil.format.obj = soil.format.obj,
                        test.file.path = test.file.path,
                        status = 0, mode = NULL,
                        is.absorbance = F, is.reflectance = F, is.transmittance = F,
                        is.descending = F, num.data.rows = 2151,
                        wavenumbers = c(350, 2500),
                        intensities = c(0.09452194, 0.11418717),
                        metadata.length = 1)

  testthat::expect_equal(object = result$standardised.metadata$spectra_wavesignature_units,
                         expected = "nm")

  testthat::expect_equal(object = result$standardised.metadata$Sample_ID,
                         expected = "EXA_01")
})

test_that("Read non-existent CSIRO SCANS file", {
  path <- "nothing at all"
  result <- soil.format.obj$read(path)

  testthat::expect_false(result$status == 0)
  testthat::expect_null(result$spec.df)
  testthat::expect_null(result$meta.df)
  testthat::expect_null(result$mode)
})
