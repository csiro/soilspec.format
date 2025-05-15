# ASD binary format unit tests

source("common_test.R", local = T)

test_that("Get ASD binary example file path", {
  expected <- system.file("extdata", "ASDBinary",
                          "example.asd", package = "soilspec.format")

  actual <- soilspec.format::asd.binary.file.path()

  testthat::expect_equal(object = actual, expected = expected)
})

test_that("Read ASD binary example file", {
  suppressWarnings({
    result <- common_test(soil.format.obj = soilspec.format::ASDBinary$new(),
                        test_file_path = soilspec.format::asd.binary.file.path(),
                        status = 0, mode = "reflectance",
                        is.absorbance = F, is.reflectance = T, is.transmittance = F,
                        is.descending = F, num.data.rows = 2151,
                        wavenumbers = c(350, 2500),
                        intensities = c(0.05269, 0.3073),
                        metadata.length = 31)

    testthat::expect_equal(object = as.character(result$allInstrumentMetadata$instrument),
                           expected = "FieldSpec FR")
  })
})

test_that("Read non-existent ASD binary file", {
  asd.binary <- soilspec.format::ASDBinary$new()

  path <- "nothing at all"
  result <- asd.binary$read(path)

  testthat::expect_false(result$status == 0)
  testthat::expect_null(result$spec.df)
  testthat::expect_null(result$meta.df)
  testthat::expect_null(result$mode)
})
