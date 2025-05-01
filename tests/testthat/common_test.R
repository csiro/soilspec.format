# Common test functions

# This function takes parameters relating to the values under common test,
# asserts their equality with respect to actual results, and returns the
# results object.
common_test <- function(soil.format.obj, test_file_path,
                        status, mode,
                        is.absorbance, is.reflectance, is.transmittance,
                        is.descending, num.data.rows,
                        wavenumbers, intensities,
                        metadata.length) {

  result <- soil.format.obj$read(test_file_path)

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
                         expected = wavenumbers[1], tolerance = 1e-4)

  testthat::expect_equal(object = result$data[1,]$intensity,
                         expected = intensities[1], tolerance = 1e-4)

  last.index <- nrow(result$data)
  testthat::expect_equal(object = result$data[last.index,]$wavenumber,
                         expected = wavenumbers[2], tolerance = 1e-4)

  testthat::expect_equal(object = result$data[last.index,]$intensity,
                         expected = intensities[2], tolerance = 1e-4)

  testthat::expect_equal(object = length(result$allInstrumentMetadata),
                         expected = metadata.length)

  result
}
