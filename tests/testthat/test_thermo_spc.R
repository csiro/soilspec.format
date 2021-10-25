# Thermo spc unit tests

test_that("Get Thermo spc example file path", {
  expected <- system.file("extdata", "ThermoSpc",
                          "example.spc", package = "soilspec.format")

  actual <- soilspec.format::thermo.spc.file.path()

  testthat::expect_equal(object = actual, expected = expected)
})

test_that("Read Thermo spc example file", {
  spc <- soilspec.format::ThermoSpc$new()
  path <- soilspec.format::thermo.spc.file.path()
  result <- spc$read(path)

  testthat::expect_equal(object = result$status, expected = 0)

  testthat::expect_equal(object = result$mode, expected = NULL)

  testthat::expect_true(result$is.descending)

  testthat::expect_equal(object = result$origin, expected = spc$origin)

  testthat::expect_equal(object = result$type, expected = spc$type_name)

  testthat::expect_equal(object = nrow(result$data),
                         expected = 3676)

  testthat::expect_equal(object = result$data[1,]$wavenumber,
                         expected = 7800, tolerance = 1e-4)

  testthat::expect_equal(object = result$data[1,]$intensity,
                         expected = 0.615247, tolerance = 1e-4)

  last.index <- nrow(result$data)
  testthat::expect_equal(object = result$data[last.index,]$wavenumber,
                         expected = 450, tolerance = 1e-4)

  testthat::expect_equal(object = result$data[last.index,]$intensity,
                         expected = 1.201563, tolerance = 1e-4)

  testthat::expect_equal(object = length(result$metadata), expected = 0)
})

test_that("Read non-existent Thermo spc file", {
  spc <- soilspec.format::ThermoSpc$new()

  path <- "nothing at all"
  result <- spc$read(path)

  testthat::expect_false(result$status == 0)
  testthat::expect_null(result$spec.df)
  testthat::expect_null(result$meta.df)
  testthat::expect_null(result$mode)
})
