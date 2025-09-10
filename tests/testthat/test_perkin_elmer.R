# Perkin Elmer format unit tests

source("common_test.R", local = T)

soil.format.obj <- soilspec.format::PerkinElmerSP$new()

# PEPE

test.file.path <- soilspec.format::perkin.elmer.sp.pepe.file.path()

test_that("Get Perkin Elmer SP PEPE example file path", {
  expected <- system.file("extdata", "PerkinElmerPEPE",
                          "example.sp", package = "soilspec.format")

  actual <- test.file.path

  testthat::expect_equal(object = actual, expected = expected)
})

test_that("Read Perkin Elmer SP PEPE example file", {
  result <- common_soil_format_object_test(soil.format.obj = soil.format.obj,
                        test.file.path = test.file.path,
                        status = 0, mode = NULL,
                        is.absorbance = F, is.reflectance = F, is.transmittance = F,
                        is.descending = T, num.data.rows = 3676,
                        wavenumbers = c(7800, 450),
                        intensities = c(0.8553731, 1.911237),
                        metadata.length = 5)

  testthat::expect_equal(object = result$standardised.metadata$spectra_wavesignature_units,
                         expected = "wn")

  testthat::expect_equal(object = result$standardised.metadata$sample_id,
                         expected = "EXAMPLE01")

  testthat::expect_equal(object = result$all.instrument.metadata$xLabel,
                         expected = "cm-1")

  testthat::expect_equal(object = result$all.instrument.metadata$alias,
                         expected = "EXAMPLE01.sp")

  testthat::expect_equal(object = result$all.instrument.metadata[["original name"]],
                         expected = "\\\\Fssa1-adl\\URRBPUBLIC\\PERKIN~1\\MIRSPE~1\\PE-SPE~1\\EXAMPLE1.SP")
})

# PEPE / PE IR
test_that("Read non-existent Perkin Elmer file", {
  path <- "nothing at all"
  result <- soil.format.obj$read(path)

  testthat::expect_false(result$status == 0)
  testthat::expect_null(result$spec.df)
  testthat::expect_null(result$meta.df)
  testthat::expect_null(result$mode)
})

# PE IR

test.file.path <- soilspec.format::perkin.elmer.sp.peir.file.path()

test_that("Get Perkin Elmer SP PE IR example file path", {
  expected <- system.file("extdata", "PerkinElmerPEIR",
                          "example.sp", package = "soilspec.format")

  actual <- test.file.path

  testthat::expect_equal(object = actual, expected = expected)
})

test_that("Read Perkin Elmer SP PE IR example file", {
  result <- common_soil_format_object_test(soil.format.obj = soil.format.obj,
                        test.file.path = test.file.path,
                        status = 0, mode = "DIFFUSE REFLECTANCE",
                        is.absorbance = F, is.reflectance = T, is.transmittance = F,
                        is.descending = T, num.data.rows = 3676,
                        wavenumbers = c(7800, 450),
                        intensities = c(0.6823456, 1.726805),
                        metadata.length = 7)

  testthat::expect_equal(object = result$standardised.metadata$spectra_wavesignature_units,
                         expected = "wn")

  testthat::expect_equal(object = result$all.instrument.metadata$xLabel,
                         expected = "CM-1")

  testthat::expect_equal(object = result$all.instrument.metadata$name,
                         expected = "EXAMPLE1.SP")
})
