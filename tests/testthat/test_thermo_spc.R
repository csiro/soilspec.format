# Thermo spc unit tests

source("common_test.R", local = T)

soil.format.obj <- soilspec.format::ThermoSpc$new()
test.file.path <- soilspec.format::thermo.spc.file.path()

test_that("Get Thermo spc example file path", {
  expected <- system.file("extdata", "ThermoSpc",
                          "example.spc", package = "soilspec.format")

  actual <- test.file.path

  testthat::expect_equal(object = actual, expected = expected)
})

test_that("Read Thermo spc example file", {
  suppressWarnings({
    result <- common_test(soil.format.obj = soil.format.obj,
                          test.file.path = test.file.path,
                          status = 0, mode = NULL,
                          is.absorbance = F, is.reflectance = F, is.transmittance = F,
                          is.descending = T, num.data.rows = 3676,
                          wavenumbers = c(7800, 450),
                          intensities = c(0.615247, 1.201563),
                          metadata.length = 0)
  })
})

test_that("Read non-existent Thermo spc file", {
  path <- "nothing at all"
  result <- soil.format.obj$read(path)

  testthat::expect_false(result$status == 0)
  testthat::expect_null(result$spec.df)
  testthat::expect_null(result$meta.df)
  testthat::expect_null(result$mode)
})
