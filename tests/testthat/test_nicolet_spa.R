# Nicolet .spa format unit tests

source("common_test.R", local = T)

test_that("Get Nicolet spa example file path", {
  expected <- system.file("extdata", "NicoletSpa",
                          "example.spa", package = "soilspec.format")

  actual <- soilspec.format::nicolet.spa.file.path()

  testthat::expect_equal(object = actual, expected = expected)
})

test_that("Read Nicolet spa example file", {
  result <- common_test(soil.format.obj = soilspec.format::NicoletSpa$new(),
                        test_file_path = soilspec.format::nicolet.spa.file.path(),
                        status = 0, mode = "Absorbance",
                        is.absorbance = T, is.reflectance = F, is.transmittance = F,
                        is.descending = T, num.data.rows = 1971,
                        wavenumbers = c(7999.28, 404.976),
                        intensities = c(0.333132, 2.0022),
                        metadata.length = 7)

  testthat::expect_equal(object = result$allInstrumentMetadata[["Bench Serial Number"]],
                         expected = "AMM0900168")
})

test_that("Read non-existent Nicolet spa file", {
  nicolet <- soilspec.format::NicoletSpa$new()

  path <- "nothing at all"
  result <- nicolet$read(path)

  testthat::expect_false(result$status == 0)
  testthat::expect_null(result$spec.df)
  testthat::expect_null(result$meta.df)
  testthat::expect_null(result$mode)
})

test_that("Extract strings from Nicolet spa example file", {
  path <- soilspec.format::nicolet.spa.file.path()
  strings <- strings.from.spa(path)
  testthat::expect_equal(object = length(strings), expected = 13)
})

test_that("Extract key-value pairs from Nicolet spa example file", {
  path <- soilspec.format::nicolet.spa.file.path()
  key2value <- key.value.pairs(path)
  testthat::expect_equal(object = length(key2value), expected = 7)
})
