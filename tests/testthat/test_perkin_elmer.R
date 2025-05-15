# Perkin Elmer format unit tests

source("common_test.R", local = T)

# PEPE

test_that("Get Perkin Elmer SP PEPE example file path", {
  expected <- system.file("extdata", "PerkinElmerPEPE",
                          "example.sp", package = "soilspec.format")

  actual <- soilspec.format::perkin.elmer.sp.pepe.file.path()

  testthat::expect_equal(object = actual, expected = expected)
})

test_that("Read Perkin Elmer SP PEPE example file", {
  result <- common_test(soil.format.obj = soilspec.format::PerkinElmerSP$new(),
                        test_file_path = soilspec.format::perkin.elmer.sp.pepe.file.path(),
                        status = 0, mode = NULL,
                        is.absorbance = F, is.reflectance = F, is.transmittance = F,
                        is.descending = T, num.data.rows = 3676,
                        wavenumbers = c(7800, 450),
                        intensities = c(0.8553731, 1.911237),
                        metadata.length = 5)

  testthat::expect_equal(object = result$allInstrumentMetadata$xLabel,
                         expected = "cm-1")
})

# PEPE / PE IR
test_that("Read non-existent Perkin Elmer file", {
  pe <- soilspec.format::PerkinElmerSP$new()

  path <- "nothing at all"
  result <- pe$read(path)

  testthat::expect_false(result$status == 0)
  testthat::expect_null(result$spec.df)
  testthat::expect_null(result$meta.df)
  testthat::expect_null(result$mode)
})

# PE IR

test_that("Get Perkin Elmer SP PE IR example file path", {
  expected <- system.file("extdata", "PerkinElmerPEIR",
                          "example.sp", package = "soilspec.format")

  actual <- soilspec.format::perkin.elmer.sp.peir.file.path()

  testthat::expect_equal(object = actual, expected = expected)
})

test_that("Read Perkin Elmer SP PE IR example file", {
  result <- common_test(soil.format.obj = soilspec.format::PerkinElmerSP$new(),
                        test_file_path = soilspec.format::perkin.elmer.sp.peir.file.path(),
                        status = 0, mode = "DIFFUSE REFLECTANCE",
                        is.absorbance = F, is.reflectance = T, is.transmittance = F,
                        is.descending = T, num.data.rows = 3676,
                        wavenumbers = c(7800, 450),
                        intensities = c(0.6823456, 1.726805),
                        metadata.length = 7)

  testthat::expect_equal(object = result$allInstrumentMetadata$xLabel,
                         expected = "CM-1")
})
