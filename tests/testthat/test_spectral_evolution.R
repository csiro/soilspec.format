# Spectral Evolution format unit tests

source("common_test.R", local = T)

soil.format.obj <- soilspec.format::SpectralEvolution$new()
test.file.path <- soilspec.format::spectral.evolution.file.path()

test_that("Get Spectral Evolution example file path", {
  expected <- system.file("extdata", "SpectralEvolution",
                          "example.sed", package = "soilspec.format")

  actual <- test.file.path

  testthat::expect_equal(object = actual, expected = expected)
})

test_that("Read Spectral Evolution example file", {
  result <- common_soil_format_object_test(soil.format.obj = soil.format.obj,
                        test.file.path = test.file.path,
                        status = 0, mode = "REFLECTANCE",
                        is.absorbance = F, is.reflectance = T, is.transmittance = F,
                        is.descending = F, num.data.rows = 2151,
                        wavenumbers = c(350, 2500),
                        intensities = c(0.069635, 0.105154),
                        metadata.length = 25)

  testthat::expect_equal(object = result$standardisedMetadata$spectra_wavesignature_units,
                         expected = "W/m^2/sr/nm")
})

test_that("Read non-existent Spectral Evolution file", {
  path <- "nothing at all"
  result <- soil.format.obj$read(path)

  testthat::expect_false(result$status == 0)
  testthat::expect_null(result$spec.df)
  testthat::expect_null(result$meta.df)
  testthat::expect_null(result$mode)
})
