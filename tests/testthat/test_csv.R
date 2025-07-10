# CSV format unit tests

source("common_test.R", local = T)

soil.format.obj <- soilspec.format::CSV$new()
test.file.path <- soilspec.format::csv.file.path()

test_that("Get CSV example file path", {
  expected <- system.file("extdata", "CSV",
                          "example.csv", package = "soilspec.format")

  actual <- test.file.path

  testthat::expect_equal(object = actual, expected = expected)
})

test_that("Read CSV example file", {
  result <- common_soil_format_object_test(soil.format.obj = soil.format.obj,
                        test.file.path = test.file.path,
                        status = 0, mode = NULL,
                        is.absorbance = F, is.reflectance = F, is.transmittance = F,
                        is.descending = T, num.data.rows = 4830,
                        wavenumbers = c(7497.58131, 598.60643),
                        intensities = c(0.92693, 0.03152),
                        metadata.length = 0)
})

test_that("Read non-existent CSV file", {
  path <- "nothing at all"
  result <- soil.format.obj$read(path)

  testthat::expect_false(result$status == 0)
  testthat::expect_null(result$spec.df)
  testthat::expect_null(result$meta.df)
  testthat::expect_null(result$mode)
})

# TODO: add tests for header skip/no header, different source.col.names, mode
