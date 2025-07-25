# Common test functions

tolerance <- 1e-4

# This soil format object centric function takes parameters relating to the
# values under test, asserts their equality with respect to actual results,
# and returns the results object.
common_soil_format_object_test <- function(soil.format.obj,
                                           test.file.path,
                                           status, mode,
                                           is.absorbance,
                                           is.reflectance,
                                           is.transmittance,
                                           is.descending,
                                           num.data.rows,
                                           wavenumbers, intensities,
                                           metadata.length) {

  result <- soil.format.obj$read(test.file.path)

  testthat::expect_equal(object = result$status, expected = status)

  testthat::expect_equal(object = result$mode, expected = mode)

  testthat::expect_equal(object = result$is.absorbance,
                         expected = is.absorbance)

  testthat::expect_equal(object = result$is.reflectance,
                         expected = is.reflectance)

  testthat::expect_equal(object = result$is.transmittance,
                         expected = is.transmittance)

  testthat::expect_equal(object = result$is.descending,
                         expected = is.descending)

  testthat::expect_equal(object = result$origin,
                         expected = soil.format.obj$origin)

  testthat::expect_equal(object = result$type,
                         expected = soil.format.obj$type_name)

  testthat::expect_equal(object = nrow(result$data),
                         expected = num.data.rows)

  testthat::expect_equal(object = result$data[1,]$wavenumber,
                         expected = wavenumbers[1], tolerance = tolerance)

  testthat::expect_equal(object = result$data[1,]$intensity,
                         expected = intensities[1], tolerance = tolerance)

  last.index <- nrow(result$data)
  testthat::expect_equal(object = result$data[last.index,]$wavenumber,
                         expected = wavenumbers[2], tolerance = tolerance)

  testthat::expect_equal(object = result$data[last.index,]$intensity,
                         expected = intensities[2], tolerance = tolerance)

  testthat::expect_equal(object = length(result$allInstrumentMetadata),
                         expected = metadata.length)

  result
}

# This test function takes parameters relating to the values under
# test and asserts their equality with respect to actual results
# for format specific and generic reader functions.
common_read_test <- function(path, read.function, suffix,
                             is.descending, num.rows,
                             is.absorbance = F, is.reflectance = F, is.transmittance = F,
                             source.col.names = c("wavenumber", "intensity")) {

  # read.soilspec function test

  suppressWarnings({
    if (suffix == ".csv") {
      result <- soilspec.format::read.soilspec.csv(path,
                                                   is.absorbance,
                                                   is.reflectance,
                                                   is.transmittance,
                                                   source.col.names)
    } else {
      result <- soilspec.format::read.soilspec(path)
    }
  })

  testthat::expect_equal(object = result$status,
                         expected = 0)

  testthat::expect_equal(object = result$is.descending,
                         expected = is.descending)

  testthat::expect_equal(object = nrow(result$data),
                         expected = num.rows)

  # read.soilspec.with.suffix function test with
  # lower case and upper case suffix

  suppressWarnings({
    if (suffix == ".csv") {
      result <- soilspec.format::read.soilspec.with.suffix(path, suffix,
                                                           is.absorbance,
                                                           is.reflectance,
                                                           is.transmittance,
                                                           source.col.names)
    } else {
      result <- soilspec.format::read.soilspec.with.suffix(path, suffix)
    }
  })

  testthat::expect_equal(object = result$status, expected = 0)

  suppressWarnings({
    if (suffix == ".csv") {
      result <- soilspec.format::read.soilspec.with.suffix(path,
                                                           str_to_upper(suffix),
                                                           is.absorbance,
                                                           is.reflectance,
                                                           is.transmittance,
                                                           source.col.names)
    } else {
      result <-
        soilspec.format::read.soilspec.with.suffix(path, str_to_upper(suffix))
    }
  })

  testthat::expect_equal(object = result$status, expected = 0)

  # format specific reader function (read.X) test

  suppressWarnings({
    if (suffix == ".csv") {
      result <- read.function(path,
                              is.absorbance, is.reflectance, is.transmittance,
                              source.col.names)
    } else {
      result <- read.function(path)
    }
  })

  testthat::expect_equal(object = result$status, expected = 0)

  # attempt to read file with invalid format using generic read function

  unknown.file <- soilspec.format::unknown.file.path()
  invalid.format.path <- stringr::str_replace(unknown.file, pattern=".xyz",
                                              replacement = suffix)
  result <- soilspec.format::read.soilspec(invalid.format.path)
  testthat::expect_equal(object = result$status, expected = 2)
}
