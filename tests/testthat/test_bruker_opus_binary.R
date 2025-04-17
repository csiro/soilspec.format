# Bruker Opus Binary unit tests

test_that("Get Bruker Opus Binary absorbance file path", {
  expected <- system.file("extdata", "BrukerOpusBinary",
                          "test_abs.0", package = "soilspec.format")

  actual <- soilspec.format::bruker.opus.binary.test.abs.file.path()

  testthat::expect_equal(object = actual, expected = expected)
})

test_that("Read Bruker Opus Binary absorbance file", {
  bruker <- soilspec.format::BrukerOpusBinary$new()
  path <- soilspec.format::bruker.opus.binary.test.abs.file.path()

  suppressWarnings({
    result <- bruker$read(path)
  })

  testthat::expect_equal(object = result$status, expected = 0)

  testthat::expect_equal(object = result$mode, expected = "AB")

  testthat::expect_true(result$is.absorbance)
  testthat::expect_equal(object = result$is.reflectance, expected = FALSE)
  testthat::expect_equal(object = result$is.transmittance, expected = FALSE)

  testthat::expect_true(result$is.descending)

  testthat::expect_equal(object = result$origin, expected = bruker$origin)

  testthat::expect_equal(object = result$type, expected = bruker$type_name)

  testthat::expect_equal(object = nrow(result$data),
                         expected = 4827)

  testthat::expect_equal(object = result$data[1,]$wavenumber,
                         expected = 7498.2, tolerance = 1e-4)

  testthat::expect_equal(object = result$data[1,]$intensity,
                         expected = 0.1366372, tolerance = 1e-4)

  last.index <- nrow(result$data)
  testthat::expect_equal(object = result$data[last.index,]$wavenumber,
                         expected = 598.9982, tolerance = 1e-4)

  testthat::expect_equal(object = result$data[last.index,]$intensity,
                         expected = 1.656751, tolerance = 1e-4)

  testthat::expect_equal(object = length(result$allInstrumentMetadata), expected = 18)

  testthat::expect_equal(object = stringr::str_to_lower(result$allInstrumentMetadata$instr_name_range),
                         expected = "invenio-s-mir")
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
    result <- bruker$read(path)
  })

  testthat::expect_equal(object = result$status, expected = 0)

  testthat::expect_equal(object = result$mode, expected = "RFL")

  testthat::expect_true(result$is.reflectance)
  testthat::expect_equal(object = result$is.absorbance, expected = FALSE)
  testthat::expect_equal(object = result$is.transmittance, expected = FALSE)

  testthat::expect_true(result$is.descending)

  testthat::expect_equal(object = result$origin, expected = bruker$origin)

  testthat::expect_equal(object = result$type, expected = bruker$type_name)

  testthat::expect_equal(object = nrow(result$data),
                         expected = 1708)

  testthat::expect_equal(object = result$data[1,]$wavenumber,
                         expected = 3997.597, tolerance = 1e-4)

  testthat::expect_equal(object = result$data[1,]$intensity,
                         expected = 0.09282638, tolerance = 1e-4)

  last.index <- nrow(result$data)
  testthat::expect_equal(object = result$data[last.index,]$wavenumber,
                         expected = 498.1621, tolerance = 1e-4)

  testthat::expect_equal(object = result$data[last.index,]$intensity,
                         expected = 0.02004875, tolerance = 1e-6)

  testthat::expect_equal(object = length(result$allInstrumentMetadata), expected = 18)

  testthat::expect_equal(object = stringr::str_to_lower(result$allInstrumentMetadata$instr_name_range),
                         expected = "alpha ii-mir")
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
