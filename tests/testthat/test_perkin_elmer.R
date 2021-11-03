# Perkin Elmer .sp unit tests

# PEPE

test_that("Get Perkin Elmer SP PEPE example file path", {
  expected <- system.file("extdata", "PerkinElmerPEPE",
                          "example.sp", package = "soilspec.format")

  actual <- soilspec.format::perkin.elmer.sp.pepe.file.path()

  testthat::expect_equal(object = actual, expected = expected)
})

test_that("Read Perkin Elmer SP PEPE example file", {
  pe <- soilspec.format::PerkinElmerSP$new()
  path <- soilspec.format::perkin.elmer.sp.pepe.file.path()
  result <- pe$read(path)

  testthat::expect_equal(object = result$status, expected = 0)

  testthat::expect_equal(object = result$mode, expected = NULL)

  testthat::expect_true(result$is.descending)

  testthat::expect_equal(object = result$origin, expected = pe$origin)

  testthat::expect_equal(object = result$type, expected = pe$type_name)

  testthat::expect_equal(object = nrow(result$data),
                         expected = 3676)

  testthat::expect_equal(object = result$data[1,]$wavenumber,
                         expected = 7800, tolerance = 1e-4)

  testthat::expect_equal(object = result$data[1,]$intensity,
                         expected = 0.8553731, tolerance = 1e-4)

  last.index <- nrow(result$data)
  testthat::expect_equal(object = result$data[last.index,]$wavenumber,
                         expected = 450, tolerance = 1e-4)

  testthat::expect_equal(object = result$data[last.index,]$intensity,
                         expected = 1.911237, tolerance = 1e-4)

  testthat::expect_equal(object = length(result$metadata), expected = 5)

  testthat::expect_equal(object = result$metadata[["xLabel"]],
                         expected = "cm-1")
})

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

# TODO
