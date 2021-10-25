# CSV unit tests

test_that("Get CSV example file path", {
  expected <- system.file("extdata", "BrukerCSV",
                          "example.csv", package = "soilspec.format")

  actual <- soilspec.format::csv.file.path()

  testthat::expect_equal(object = actual, expected = expected)
})

test_that("Read CSV example file", {
  csv <- soilspec.format::CSV$new()
  path <- soilspec.format::csv.file.path()
  result <- csv$read(path)

  testthat::expect_equal(object = result$status, expected = 0)

  testthat::expect_equal(object = result$mode, expected = NULL)

  testthat::expect_true(result$is.descending)

  testthat::expect_equal(object = result$origin, expected = csv$origin)

  testthat::expect_equal(object = result$type, expected = csv$type_name)

  testthat::expect_equal(object = nrow(result$data),
                         expected = 4830)

  testthat::expect_equal(object = result$data[1,]$wavenumber,
                         expected = 7497.58131, tolerance = 1e-4)

  testthat::expect_equal(object = result$data[1,]$intensity,
                         expected = 0.92693, tolerance = 1e-4)

  last.index <- nrow(result$data)
  testthat::expect_equal(object = result$data[last.index,]$wavenumber,
                         expected = 598.60643, tolerance = 1e-4)

  testthat::expect_equal(object = result$data[last.index,]$intensity,
                         expected = 0.03152, tolerance = 1e-4)

  testthat::expect_equal(object = length(result$metadata), expected = 0)
})

test_that("Read non-existent CSV file", {
  csv <- soilspec.format::CSV$new()

  path <- "nothing at all"
  result <- csv$read(path)

  testthat::expect_false(result$status == 0)
  testthat::expect_null(result$spec.df)
  testthat::expect_null(result$meta.df)
  testthat::expect_null(result$mode)
})
