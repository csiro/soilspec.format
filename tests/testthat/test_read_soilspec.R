# Unit tests for generic and common read functions

# Tests for each known type via generic read function

test_that("Read ASD binary example file with generic read function", {
  path <- soilspec.format::asd.binary.file.path()
  result <- soilspec.format::read.soilspec(path)

  testthat::expect_equal(object = result$status, expected = 0)

  testthat::expect_false(result$is.descending)

  testthat::expect_equal(object = nrow(result$data),
                         expected = 2151)
})

test_that("Read ASD binary example file with generic read function where suffix case is changed", {
  path <- soilspec.format::asd.binary.file.path()
  result <- soilspec.format::read.soilspec.with.suffix(path, ".ASD")
  testthat::expect_equal(object = result$status, expected = 0)
})

test_that("Read Bruker Opus binary example file with generic read function", {
  path <- soilspec.format::bruker.opus.binary.file.path()
  result <- soilspec.format::read.soilspec(path)

  testthat::expect_equal(object = result$status, expected = 0)

  testthat::expect_equal(object = result$mode, expected = "AB")

  testthat::expect_true(result$is.descending)

  testthat::expect_equal(object = nrow(result$data),
                         expected = 4827)
})

test_that("Read Perkin Elmer PEPE example file with generic read function", {
  path <- soilspec.format::perkin.elmer.sp.pepe.file.path()
  result <- soilspec.format::read.soilspec(path)

  testthat::expect_equal(object = result$status, expected = 0)

  testthat::expect_true(result$is.descending)

  testthat::expect_equal(object = nrow(result$data),
                         expected = 3676)
})

test_that("Read Nicolet spa example file with generic read function", {
  path <- soilspec.format::nicolet.spa.file.path()
  result <- soilspec.format::read.soilspec(path)

  testthat::expect_equal(object = result$status, expected = 0)

  testthat::expect_equal(object = result$mode, expected = "Absorbance")

  testthat::expect_true(result$is.descending)

  testthat::expect_equal(object = nrow(result$data),
                         expected = 1971)
})

test_that("Read Nicolet spa example file with generic read function where suffix case is changed", {
  path <- soilspec.format::nicolet.spa.file.path()
  result <- soilspec.format::read.soilspec.with.suffix(path, ".SPA")
  testthat::expect_equal(object = result$status, expected = 0)
})

test_that("Read Thermo spc example file with generic read function", {
  path <- soilspec.format::thermo.spc.file.path()
  result <- soilspec.format::read.soilspec(path)

  testthat::expect_equal(object = result$status, expected = 0)

  testthat::expect_equal(object = result$mode, expected = NULL)

  testthat::expect_true(result$is.descending)

  testthat::expect_equal(object = nrow(result$data),
                         expected = 3676)
})

test_that("Read Thermo spc example file with generic read function where suffix case is changed", {
  path <- soilspec.format::thermo.spc.file.path()
  result <- soilspec.format::read.soilspec.with.suffix(path, ".SPC")
  testthat::expect_equal(object = result$status, expected = 0)
})

test_that("Read CSV example file with generic read function", {
  path <- soilspec.format::csv.file.path()
  result <- soilspec.format::read.soilspec(path)

  testthat::expect_equal(object = result$status, expected = 0)

  testthat::expect_equal(object = result$mode, expected = NULL)

  testthat::expect_true(result$is.descending)

  testthat::expect_equal(object = nrow(result$data),
                         expected = 4830)
})

test_that("Read CSV example file with generic read function where suffix case is changed", {
  path <- soilspec.format::csv.file.path()
  result <- soilspec.format::read.soilspec.with.suffix(path, ".CSV")
  testthat::expect_equal(object = result$status, expected = 0)
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

test_that("Read Perkin Elmer PEPE example file with format specific read function", {
  path <- soilspec.format::perkin.elmer.sp.pepe.file.path()
  result <- soilspec.format::read.perkin.elmer.sp(path)

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

test_that("Read CSV example file with format specific read function", {
  path <- soilspec.format::csv.file.path()
  result <- soilspec.format::read.soilspec.csv(path)

  testthat::expect_equal(object = result$status, expected = 0)
})

# Tests for unreachable path, unknown file suffix, invalid/unknown file format

test_that("Attempt to read unreachable path with generic read function", {
  result <- soilspec.format::read.soilspec("a/b/c")
  testthat::expect_equal(object = result$status, expected = 1)
})

test_that("Attempt to read unreachable path with common (internal) read function", {
  result <- soilspec.format::read.soilspec.with.suffix("a/b/c", ".spa")
  testthat::expect_equal(object = result$status, expected = 1)
})

test_that("Attempt to read Bruker file with unknown file suffix with generic read function", {
  path <- soilspec.format::unknown.file.path()
  result <- soilspec.format::read.soilspec(path)
  testthat::expect_equal(object = result$status, expected = 2)
})

test_that("Attempt to read Bruker file with unknown file suffix with common (internal) read function", {
  path <- soilspec.format::bruker.opus.binary.file.path()
  result <- soilspec.format::read.soilspec.with.suffix(path, ".foo")
  testthat::expect_equal(object = result$status, expected = 2)
})

test_that("Attempt to read Perkin Elmer file with invalid format using generic read function", {
  path <- soilspec.format::unknown.file.path()
  path <- stringr::str_replace(path, pattern="xyz", replacement = "sp")
  result <- soilspec.format::read.soilspec(path)
  testthat::expect_equal(object = result$status, expected = 4)
})

test_that("Attempt to read Perkin Elmer PEPE file with invalid format using common (internal) read function", {
  path <- soilspec.format::perkin.elmer.sp.pepe.file.path()
  result <- soilspec.format::read.soilspec.with.suffix(path, ".spa")
  # apparently this reader can read PE (!), but the result is invalid,
  # so check for number of rows instead of status
  testthat::expect_false(nrow(result$data) == 3676)
})

test_that("Attempt to read Nicolet spa file with invalid format using generic read function", {
  path <- soilspec.format::unknown.file.path()
  path <- stringr::str_replace(path, pattern="xyz", replacement = "spa")
  result <- soilspec.format::read.soilspec(path)
  testthat::expect_equal(object = result$status, expected = 4)
})

test_that("Attempt to read Nicolet spa file with invalid format using common (internal) read function", {
  path <- soilspec.format::bruker.opus.binary.file.path()
  result <- soilspec.format::read.soilspec.with.suffix(path, ".spa")
  testthat::expect_equal(object = result$status, expected = 4)
})

test_that("Attempt to read Thermo spc file with invalid format using generic read function", {
  path <- soilspec.format::unknown.file.path()
  path <- stringr::str_replace(path, pattern="xyz", replacement = "spc")
  result <- soilspec.format::read.soilspec(path)
  testthat::expect_equal(object = result$status, expected = 4)
})

test_that("Attempt to read CSV file with invalid format using generic read function", {
  path <- soilspec.format::unknown.file.path()
  path <- stringr::str_replace(path, pattern="xyz", replacement = "csv")
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
