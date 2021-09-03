# Nicolet unit tests

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

  testthat::expect_equal(object = length(result$metadata),
                         expected = 1)

  # TODO: test extract metadata; currentlly empty vector
})
