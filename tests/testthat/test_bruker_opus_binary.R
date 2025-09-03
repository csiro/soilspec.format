# Bruker Opus Binary format unit tests

source("common_test.R", local = T)

test_that("Get Bruker Opus Binary absorbance file path", {
  expected <- system.file("extdata", "BrukerOpusBinary",
                          "test_abs.0", package = "soilspec.format")

  actual <- soilspec.format::bruker.opus.binary.test.abs.file.path()

  testthat::expect_equal(object = actual, expected = expected)
})

test_that("Read Bruker Opus Binary absorbance file", {
  suppressWarnings({
    result <- common_soil_format_object_test(soil.format.obj = soilspec.format::BrukerOpusBinary$new(),
                          test.file.path = soilspec.format::bruker.opus.binary.test.abs.file.path(),
                          status = 0, mode = "AB",
                          is.absorbance = T, is.reflectance = F, is.transmittance = F,
                          is.descending = T, num.data.rows = 4827,
                          wavenumbers = c(7498.2, 598.9982),
                          intensities = c(0.21754, 1.74638),
                          metadata.length = 18)
  })

  testthat::expect_equal(object = stringr::str_to_lower(result$all.instrument.metadata$instr_name_range),
                         expected = "invenio-s-mir")

  testthat::expect_equal(object = result$standardised.metadata$sample.id,
                         expected = "M1")

  testthat::expect_equal(object = result$standardised.metadata$spectra.id,
                         expected = "M1_2021-01-19 04:38:09")
})

test_that("Get Bruker Opus Binary reflectance file path", {
  expected <- system.file("extdata", "BrukerOpusBinary",
                          "test_refl.0", package = "soilspec.format")

  actual <- soilspec.format::bruker.opus.binary.test.refl.file.path()

  testthat::expect_equal(object = actual, expected = expected)
})

test_that("Read Bruker Opus Binary reflectance file", {
  bruker <- soilspec.format::BrukerOpusBinary$new()
  path <- soilspec.format::bruker.opus.binary.test.refl.file.path()

  suppressWarnings({
    suppressWarnings({
      result <- common_soil_format_object_test(soil.format.obj = soilspec.format::BrukerOpusBinary$new(),
                            test.file.path = soilspec.format::bruker.opus.binary.test.refl.file.path(),
                            status = 0, mode = "RFL",
                            is.absorbance = F, is.reflectance = T, is.transmittance = F,
                            is.descending = T, num.data.rows = 1708,
                            wavenumbers = c(3997.5973, 498.1621),
                            intensities = c(0.09282638, 0.02004875),
                            metadata.length = 18)
    })
  })

  testthat::expect_equal(object = stringr::str_to_lower(result$all.instrument.metadata$instr_name_range),
                         expected = "alpha ii-mir")

  std.meta <- result$standardised.metadata

  # check standard metadata potentially affected by "Bruker standard metadata: changes required" issue
  testthat::expect_equal(object = std.meta$date.time, expected = "2024-03-07 11:48:11")
  testthat::expect_equal(object = std.meta$sample.id, expected = "Example_123")
  testthat::expect_equal(object = std.meta$spectra.id, expected = "Example_123_2024-03-07 11:48:11")
  testthat::expect_equal(object = std.meta$spectra_source_file_name, expected = "test_refl.0")
  testthat::expect_equal(object = std.meta$instrument_min_wavelength, expected = 498.1621, tolerance=tolerance)
  testthat::expect_equal(object = std.meta$instrument_max_wavelength, expected = 3997.5973, tolerance=tolerance)
})

test_that("Read non-existent Bruker Opus Binary file", {
  bruker <- soilspec.format::BrukerOpusBinary$new()

  path <- "nothing at all"
  result <- bruker$read(path)

  testthat::expect_false(result$status == 0)
  testthat::expect_null(result$spec.df)
  testthat::expect_null(result$meta.df)
  testthat::expect_null(result$mode)
})
