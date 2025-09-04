# ASD binary format unit tests

source("common_test.R", local = T)

soil.format.obj <- soilspec.format::ASDBinary$new()
test.file.path <- soilspec.format::asd.binary.file.path()

test_that("Get ASD binary example file path", {
  expected <- system.file("extdata", "ASDBinary",
                          "example.asd", package = "soilspec.format")

  actual <- test.file.path

  testthat::expect_equal(object = actual, expected = expected)
})

test_that("Read ASD binary example file", {
  suppressWarnings({
    result <- common_soil_format_object_test(soil.format.obj = soil.format.obj,
                          test.file.path = test.file.path,
                          status = 0, mode = "reflectance",
                          is.absorbance = F, is.reflectance = T, is.transmittance = F,
                          is.descending = F, num.data.rows = 2151,
                          wavenumbers = c(350, 2500),
                          intensities = c(0.05269, 0.3073),
                          metadata.length = 31)

    testthat::expect_equal(object = result$standardised.metadata$spectra_wavesignature_units,
                           expected = "wn")

    testthat::expect_equal(object = as.character(result$all.instrument.metadata$instrument),
                           expected = "FieldSpec FR")
  })
})

test_that("Read non-existent ASD binary file", {
  path <- "nothing at all"
  result <- soil.format.obj$read(path)

  testthat::expect_false(result$status == 0)
  testthat::expect_null(result$spec.df)
  testthat::expect_null(result$meta.df)
  testthat::expect_null(result$mode)
})
