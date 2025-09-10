# Nicolet .spa format unit tests

source("common_test.R", local = T)

soil.format.obj <- soilspec.format::NicoletSpa$new()
test.file.path <- soilspec.format::nicolet.spa.file.path()

test_that("Get Nicolet spa example file path", {
  expected <- system.file("extdata", "NicoletSpa",
                          "example.spa", package = "soilspec.format")

  actual <- test.file.path

  testthat::expect_equal(object = actual, expected = expected)
})

test_that("Read Nicolet spa example file", {
  result <- common_soil_format_object_test(soil.format.obj = soil.format.obj,
                        test.file.path = test.file.path,
                        status = 0, mode = "Absorbance",
                        is.absorbance = T, is.reflectance = F, is.transmittance = F,
                        is.descending = T, num.data.rows = 1971,
                        wavenumbers = c(7999.28, 404.976),
                        intensities = c(0.333132, 2.0022),
                        metadata.length = 7)

  testthat::expect_equal(object = result$standardised.metadata$spectra_wavesignature_units,
                         expected = "wn")

  testthat::expect_equal(object = result$all.instrument.metadata[["Bench Serial Number"]],
                         expected = "AMM0900168")

  testthat::expect_equal(object = result$all.instrument.metadata[["Background file"]],
                         expected = "C:\\AutoPRO5\\Spectra\\2025Exam\\Example1\\Example12345.spa")
})

test_that("Read non-existent Nicolet spa file", {
  path <- "nothing at all"
  result <- soil.format.obj$read(path)

  testthat::expect_false(result$status == 0)
  testthat::expect_null(result$spec.df)
  testthat::expect_null(result$meta.df)
  testthat::expect_null(result$mode)
})

test_that("Extract strings from Nicolet spa example file", {
  strings <- soilspec.format::strings.from.spa(test.file.path)
  testthat::expect_equal(object = length(strings), expected = 13)
})

test_that("Extract key-value pairs from Nicolet spa example file", {
  key2value <- soilspec.format::key.value.pairs(test.file.path)
  testthat::expect_equal(object = length(key2value), expected = 7)
})
