# Unit tests for generic and common read functions.
# Each of the particular soil format object tests are more detailed.
# The question we are asking here is: do the high-level reader functions work?

source("common_test.R", local = T)

# * Tests for each known type via generic read function *

test_that("Read ASD binary example file with generic read function", {
  common_read_test(
    path = soilspec.format::asd.binary.file.path(),
    read.function = soilspec.format::read.asd.binary,
    suffix = ".asd", is.descending = F, num.rows = 2151)
})

test_that("Read ASD SCO binary example file with generic read function", {
  common_read_test(
    path = soilspec.format::asd.sco.binary.file.path(),
    read.function = soilspec.format::read.asd.sco.binary,
    suffix = ".sco", is.descending = F, num.rows = 2151)
})

test_that("Read Bruker Opus binary example file with generic read function", {
  common_read_test(
    path = soilspec.format::bruker.opus.binary.file.path(),
    read.function = soilspec.format::read.bruker.opus.binary,
    suffix = ".0", is.descending = T, num.rows = 4827)
})

test_that("Read Perkin Elmer PEPE example file with generic read function", {
  common_read_test(
    path = soilspec.format::perkin.elmer.sp.pepe.file.path(),
    read.function = soilspec.format::read.perkin.elmer.sp,
    suffix = ".sp", is.descending = T, num.rows = 3676)
})

test_that("Read Perkin Elmer PE IR example file with generic read function", {
  common_read_test(
    path = soilspec.format::perkin.elmer.sp.peir.file.path(),
    read.function = soilspec.format::read.perkin.elmer.sp,
    suffix = ".sp", is.descending = T, num.rows = 3676)
})

test_that("Read Nicolet spa example file with generic read function", {
  common_read_test(
    path = soilspec.format::nicolet.spa.file.path(),
    read.function = soilspec.format::read.nicolet.spa,
    suffix = ".spa", is.descending = T, num.rows = 1971)
})

test_that("Read Thermo spc example file with generic read function", {
  common_read_test(
    path = soilspec.format::thermo.spc.file.path(),
    read.function = soilspec.format::read.thermo.spc,
    suffix = ".spc", is.descending = T, num.rows = 3676)
})

test_that("Read CSV test file with generic read function", {
  common_read_test(
    path = soilspec.format::csv.test.file.path(),
    read.function = soilspec.format::read.soilspec.csv,
    suffix = ".csv", is.descending = T, num.rows = 4830,
    is.absorbance = T, is.reflectance = F, is.transmittance = F,
    source.col.names = c("x", "y"))
})

test_that("Read CSV example file with generic read function", {
  common_read_test(
    path = soilspec.format::csv.file.path(),
    read.function = soilspec.format::read.soilspec.csv,
    suffix = ".csv", is.descending = T, num.rows = 4830,
    is.absorbance = T, is.reflectance = F, is.transmittance = F,
    source.col.names = c("wavenumber", "intensity"))
})

test_that("Read Hone Lab Red example file with generic read function", {
  common_read_test(
    path = soilspec.format::hone.lab.red.file.path(),
    read.function = soilspec.format::read.hone.lab.red,
    suffix = ".hlr", is.descending = F, num.rows = 513)
})

test_that("Read CSIRO SCANS example file with generic read function", {
  common_read_test(
    path = soilspec.format::csiro.scans.file.path(),
    read.function = soilspec.format::read.csiro.scans,
    suffix = ".scan", is.descending = F, num.rows = 2151)
})

#######################################
# Add tests for new formats above ^^^ #
#######################################

# * Special case tests *

# Tests for unreachable path

test_that("Attempt to read unreachable path with generic read function", {
  result <- soilspec.format::read.soilspec("a/b/c")
  testthat::expect_equal(object = result$status, expected = 1)
})

test_that("Attempt to read unreachable path with suffix-based function", {
  result <- soilspec.format::read.soilspec.with.suffix("a/b/c", ".spa")
  testthat::expect_equal(object = result$status, expected = 1)
})

# Tests for unknown file suffix

test_that("Attempt to read a file with unknown suffix", {
  path <- soilspec.format::bruker.opus.binary.file.path()
  result <- soilspec.format::read.soilspec.with.suffix(path, ".foo")
  testthat::expect_equal(object = result$status, expected = 2)
})
