# CSV unit tests

source("common_test.R", local = T)

test_that("Get CSV example file path", {
  expected <- system.file("extdata", "BrukerCSV",
                          "example.csv", package = "soilspec.format")

  actual <- soilspec.format::csv.file.path()

  testthat::expect_equal(object = actual, expected = expected)
})

test_that("Read CSV example file", {
  result <- common_test(soil.format.obj = soilspec.format::CSV$new(),
                        test_file_path = soilspec.format::csv.file.path(),
                        status = 0, mode = NULL,
                        is.absorbance = F, is.reflectance = F, is.transmittance = F,
                        is.descending = T, num.data.rows = 4830,
                        wavenumbers = c(7497.58131, 598.60643),
                        intensities = c(0.92693, 0.03152),
                        metadata.length = 0)
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
