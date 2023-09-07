# ASD binary unit tests

test_that("Get ASD binary example file path", {
  expected <- system.file("extdata", "ASDBinary",
                          "example.asd", package = "soilspec.format")

  actual <- soilspec.format::asd.binary.file.path()

  testthat::expect_equal(object = actual, expected = expected)
})

test_that("Read ASD binary example file", {
  asd.binary <- soilspec.format::ASDBinary$new()
  path <- soilspec.format::asd.binary.file.path()

  suppressWarnings({
    result <- asd.binary$read(path)
  })

  testthat::expect_equal(object = result$status, expected = 0)

  testthat::expect_equal(object = result$mode, expected = "reflectance")

  testthat::expect_false(result$is.descending)

  testthat::expect_equal(object = result$origin, expected = asd.binary$origin)

  testthat::expect_equal(object = result$type, expected = asd.binary$type_name)

  testthat::expect_equal(object = nrow(result$data),
                         expected = 2151)

  testthat::expect_equal(object = result$data[1,]$wavenumber,
                         expected = 350, tolerance = 1e-4)

  testthat::expect_equal(object = result$data[1,]$intensity,
                         expected = 0.0543292, tolerance = 1e-4)

  last.index <- nrow(result$data)
  testthat::expect_equal(object = result$data[last.index,]$wavenumber,
                         expected = 2500, tolerance = 1e-4)

  testthat::expect_equal(object = result$data[last.index,]$intensity,
                         expected = 0.303005, tolerance = 1e-4)

  testthat::expect_equal(object = length(result$metadata), expected = 31)

  testthat::expect_equal(object = as.character(result$metadata$instrument),
                         expected = "FieldSpec FR")
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
