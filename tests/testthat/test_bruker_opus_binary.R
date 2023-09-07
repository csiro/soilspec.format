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

  suppressWarnings({
    result <- bruker$read(path)
  })

  testthat::expect_equal(object = result$status, expected = 0)

  testthat::expect_equal(object = result$mode, expected = "AB")

  testthat::expect_true(result$is.descending)

  testthat::expect_equal(object = result$origin, expected = bruker$origin)

  testthat::expect_equal(object = result$type, expected = bruker$type_name)

  testthat::expect_equal(object = nrow(result$data),
                         expected = 4827)

  testthat::expect_equal(object = result$data[1,]$wavenumber,
                         expected = 7498.2, tolerance = 1e-4)

  testthat::expect_equal(object = result$data[1,]$intensity,
                         expected = 0.00885185, tolerance = 1e-4)

  last.index <- nrow(result$data)
  testthat::expect_equal(object = result$data[last.index,]$wavenumber,
                         expected = 598.9982, tolerance = 1e-4)

  testthat::expect_equal(object = result$data[last.index,]$intensity,
                         expected = 0.001057828, tolerance = 1e-4)

  testthat::expect_equal(object = length(result$metadata), expected = 18)

  testthat::expect_equal(object = stringr::str_to_lower(result$metadata$instr_name_range),
                         expected = "invenio-s-mir")
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
