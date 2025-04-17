# ASD SCO binary unit tests

test_that("Get ASD SCO binary example file path", {
  expected <- system.file("extdata", "ASDScoBinary",
                          "example.asd.sco", package = "soilspec.format")

  actual <- soilspec.format::asd.sco.binary.file.path()

  testthat::expect_equal(object = actual, expected = expected)
})

test_that("Read ASD SCO binary example file", {
  asd.sco.binary <- soilspec.format::ASDScoBinary$new()
  path <- soilspec.format::asd.sco.binary.file.path()

  suppressWarnings({
      result <- asd.sco.binary$read(path)
  })

  testthat::expect_equal(object = result$status, expected = 0)

  testthat::expect_equal(object = result$mode, expected = NULL)
  testthat::expect_equal(object = result$is.absorbance, expected = FALSE)
  testthat::expect_equal(object = result$is.reflectance, expected = FALSE)
  testthat::expect_equal(object = result$is.transmittance, expected = FALSE)

  testthat::expect_false(result$is.descending)

  testthat::expect_equal(object = result$origin, expected = asd.sco.binary$origin)

  testthat::expect_equal(object = result$type, expected = asd.sco.binary$type_name)

  testthat::expect_equal(object = nrow(result$data),
                         expected = 2151)

  testthat::expect_equal(object = result$data[1,]$wavenumber,
                         expected = 350, tolerance = 1e-4)

  testthat::expect_equal(object = result$data[1,]$intensity,
                         expected = 0.1168832, tolerance = 1e-4)

  last.index <- nrow(result$data)
  testthat::expect_equal(object = result$data[last.index,]$wavenumber,
                         expected = 2500, tolerance = 1e-4)

  testthat::expect_equal(object = result$data[last.index,]$intensity,
                         expected = 0.5238607, tolerance = 1e-4)

  testthat::expect_equal(object = length(result$allInstrumentMetadata), expected = 1)
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
