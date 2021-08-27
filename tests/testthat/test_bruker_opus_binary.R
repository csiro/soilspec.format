# Bruker Opus Binary unit tests

test_that("Get Bruker Opus Binary example file path", {
  expected <- system.file("extdata", "BrukerOpusBinary",
                          "example.0", package = "soilspec.format")

  actual <- soilspec.format::bruker.opus.binary.file.path()

  testthat::expect_equal(object = actual, expected = expected)
})

test_that("Read Bruker Opus Binary example file", {
  bruker <- soilspec.format::BrukerOpusBinary$new()

  path <- soilspec.format::bruker.opus.binary.file.path()
  result <- bruker$read(path)

  testthat::expect_equal(object = nrow(result$data),
                         expected = 4827)

  testthat::expect_equal(object = result$data[1,]$wavenumber,
                   expected = 7498.2, tolerance = 1e-4)

  testthat::expect_equal(object = result$data[1,]$intensity,
                         expected = 0.1366372, tolerance = 1e-4)

  last <- nrow(result$data)
  testthat::expect_equal(object = result$data[last,]$wavenumber,
                         expected = 598.9982, tolerance = 1e-4)

  testthat::expect_equal(object = result$data[last,]$intensity,
                         expected = 1.656751, tolerance = 1e-4)

  testthat::expect_equal(object = length(result$metadata),
                         expected = 1)

  testthat::expect_equal(object =
                           grep(result$metadata,
                                pattern = "invenio-s-mir"),
                         expected = 1)
})
