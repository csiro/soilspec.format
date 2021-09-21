# Nicolet .spa unit tests

test_that("Get Nicolet spa example file path", {
  expected <- system.file("extdata", "NicoletSpa",
                          "example.spa", package = "soilspec.format")

  actual <- soilspec.format::nicolet.spa.file.path()

  testthat::expect_equal(object = actual, expected = expected)
})

test_that("Read Nicolet spa example file", {
  nicolet <- soilspec.format::NicoletSpa$new()
  path <- soilspec.format::nicolet.spa.file.path()
  result <- nicolet$read(path)

  testthat::expect_equal(object = result$status, expected = 0)

  testthat::expect_equal(object = result$mode, expected = "absorbance")

  testthat::expect_equal(object = nrow(result$data),
                         expected = 1971)

  testthat::expect_equal(object = result$data[1,]$wavenumber,
                         expected = 7999.28, tolerance = 1e-4)

  testthat::expect_equal(object = result$data[1,]$intensity,
                         expected = 0.333132, tolerance = 1e-4)

  last.index <- nrow(result$data)
  testthat::expect_equal(object = result$data[last.index,]$wavenumber,
                         expected = 404.976, tolerance = 1e-4)

  testthat::expect_equal(object = result$data[last.index,]$intensity,
                         expected = 2.0022, tolerance = 1e-4)

  testthat::expect_equal(object = length(result$metadata), expected = 7)

  testthat::expect_equal(object = result$metadata[["Bench Serial Number"]],
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
  testthat::expect_null(result$units)
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
