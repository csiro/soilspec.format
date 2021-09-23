# Unit tests for generic and common read functions (read.soilspec, read.soilspec.ommon)

# Tests for each known type via generic read function

test_that("Read ASD binary example file with generic read function", {
  path <- soilspec.format::asd.binary.file.path()
  result <- soilspec.format::read.soilspec(path)

  testthat::expect_equal(object = result$status, expected = 0)

  testthat::expect_false(result$is.descending)

  testthat::expect_equal(object = nrow(result$data),
                         expected = 2151)
})

test_that("Read Bruker Opus binary example file with generic read function", {
  path <- soilspec.format::bruker.opus.binary.file.path()
  result <- soilspec.format::read.soilspec(path)

  testthat::expect_equal(object = result$status, expected = 0)

  testthat::expect_equal(object = result$mode, expected = "absorbance")

  testthat::expect_equal(object = result$units, expected = "?")

  testthat::expect_true(result$is.descending)

  testthat::expect_equal(object = nrow(result$data),
                         expected = 4827)
})

test_that("Read Nicolet spa example file with generic read function", {
  path <- soilspec.format::nicolet.spa.file.path()
  result <- soilspec.format::read.soilspec(path)

  testthat::expect_equal(object = result$status, expected = 0)

  testthat::expect_equal(object = result$mode, expected = "absorbance")

  testthat::expect_equal(object = result$units, expected = "?")

  testthat::expect_true(result$is.descending)

  testthat::expect_equal(object = nrow(result$data),
                         expected = 1971)
})

test_that("Read Thermo spc example file with generic read function", {
  path <- soilspec.format::thermo.spc.file.path()
  result <- soilspec.format::read.soilspec(path)

  testthat::expect_equal(object = result$status, expected = 0)

  testthat::expect_equal(object = result$mode, expected = "?")

  testthat::expect_equal(object = result$units, expected = "?")

  testthat::expect_true(result$is.descending)

  testthat::expect_equal(object = nrow(result$data),
                         expected = 3676)
})

# Tests for each known type via format specific functions

test_that("Read ASD binary example file with format specific read function", {
  path <- soilspec.format::asd.binary.file.path()
  result <- soilspec.format::read.asd.binary(path)

  testthat::expect_equal(object = result$status, expected = 0)
})

test_that("Read Bruker Opus binary example file with format specific read function", {
  path <- soilspec.format::bruker.opus.binary.file.path()
  result <- soilspec.format::read.bruker.opus.binary(path)

  testthat::expect_equal(object = result$status, expected = 0)
})

test_that("Read Nicolet spa example file with format specific read function", {
  path <- soilspec.format::nicolet.spa.file.path()
  result <- soilspec.format::read.nicolet.spa(path)

  testthat::expect_equal(object = result$status, expected = 0)
})

test_that("Read Thermo spc example file with format specific read function", {
  path <- soilspec.format::thermo.spc.file.path()
  result <- soilspec.format::read.thermo.spc(path)

  testthat::expect_equal(object = result$status, expected = 0)
})

# Tests for unreachable path, unknown file suffix, invalid/unknown file format

test_that("Attempt to read unreachable path with generic read function", {
  result <- soilspec.format::read.soilspec("a/b/c")
  testthat::expect_equal(object = result$status, expected = 1)
})

test_that("Attempt to read unreachable path with common (internal) read function", {
  result <- soilspec.format::read.soilspec.common("a/b/c", ".spa")
  testthat::expect_equal(object = result$status, expected = 1)
})

test_that("Attempt to read file with unknown file suffix with generic read function", {
  path <- soilspec.format::unknown.file.path()
  result <- soilspec.format::read.soilspec(path)
  testthat::expect_equal(object = result$status, expected = 2)
})

test_that("Attempt to read file with unknown file suffix with common (internal) read function", {
  path <- soilspec.format::bruker.opus.binary.file.path()
  result <- soilspec.format::read.soilspec.common(path, ".foo")
  testthat::expect_equal(object = result$status, expected = 2)
})

test_that("Attempt to read Nicolet spa file with invalid format using generic read function", {
  path <- soilspec.format::unknown.file.path()
  path <- stringr::str_replace(path, pattern="xyz", replacement = "spa")
  result <- soilspec.format::read.soilspec(path)
  testthat::expect_equal(object = result$status, expected = 4)
})

test_that("Attempt to read Nicolet spa file with invalid format using common (internal) read function", {
  path <- soilspec.format::bruker.opus.binary.file.path()
  result <- soilspec.format::read.soilspec.common(path, ".spa")
  testthat::expect_equal(object = result$status, expected = 4)
})

test_that("Attempt to read Thermo spc file with invalid format using generic read function", {
  # this test passes but generates noisy warning output;
  # enable this to check it passes!
  path <- soilspec.format::unknown.file.path()
  path <- stringr::str_replace(path, pattern="xyz", replacement = "spc")
  result <- soilspec.format::read.soilspec(path)
  testthat::expect_equal(object = result$status, expected = 4)
})

test_that("Attempt to read Bruker Opus binary file with invalid format using generic read function", {
  if (F) {
    # this test passes but generates noisy warning output;
    # enable this to check it passes!
    path <- soilspec.format::unknown.file.path()
    path <- stringr::str_replace(path, pattern="xyz", replacement = "0")
    result <- soilspec.format::read.soilspec(path)
    testthat::expect_equal(object = result$status, expected = 4)

  }
})

test_that("Attempt to read ASD binary file with invalid format using generic read function", {
  if (F) {
    # this test passes but generates noisy warning output;
    # enable this to check it passes!
    path <- soilspec.format::unknown.file.path()
    path <- stringr::str_replace(path, pattern="xyz", replacement = "asd")
    result <- soilspec.format::read.soilspec(path)
    testthat::expect_equal(object = result$status, expected = 4)
  }
})
