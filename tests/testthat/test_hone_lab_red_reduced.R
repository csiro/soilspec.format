# Hone Lab Red Reduced format unit tests

source("common_test.R", local = T)

soil.format.obj <- soilspec.format::HoneLabRedReduced$new()
test.file.path <- soilspec.format::hone.lab.red.reduced.file.path()

test_that("Get Hone Lab Red Reduced example file path", {
  expected <- system.file("extdata", "HoneLabRedReduced",
                          "31462.hlrr", package = "soilspec.format")

  actual <- test.file.path

  testthat::expect_equal(object = actual, expected = expected)
})

test_that("Read Hone Lab Red Reduced Reduced example file", {
  suppressWarnings({
    result <- common_soil_format_object_test(soil.format.obj = soil.format.obj,
                          test.file.path = test.file.path,
                          status = 0, mode = NULL,
                          is.absorbance = F, is.reflectance = F, is.transmittance = F,
                          is.descending = F, num.data.rows = 144,
                          wavenumbers = c(1347.597, 2561.417),
                          intensities = c(0.2082933, 0.1230230),
                          metadata.length = 9)
  })
})

test_that("Read non-existent Hone Lab Red Reduced file", {
  path <- "nothing at all"
  result <- soil.format.obj$read(path)

  testthat::expect_false(result$status == 0)
  testthat::expect_null(result$spec.df)
  testthat::expect_null(result$meta.df)
  testthat::expect_null(result$mode)
})
