# Generic read type function unit tests
# Tested with Little Endian storage (Intel OK, ARM can be either)
# We test current common usage (e.g. see multiple value tests).
# We could also test unsigned integers, for example.

test_that("read.int16 test", {
  con <- raw(2)
  con[1] <- as.raw(40)
  num <- read.int16(con)
  testthat::expect_equal(num, 40)
})

test_that("read.int32 test", {
  con <- raw(4)
  con[1] <- as.raw(40)
  num <- read.int32(con)
  testthat::expect_equal(num, 40)
})

test_that("read.double test", {
  con <- writeBin(as.double(40.0), raw())
  num <- read.double(con)
  testthat::expect_equal(num, as.double(40.0))
})

test_that("read.double multiple value test", {
  con <- writeBin(as.double(c(40.0, 42.0)), raw())
  nums <- read.double(con, num = 2)
  testthat::expect_equal(nums, as.double(c(40.0, 42.0)))
})

test_that("read.single test", {
  # I had trouble constructing a suitable raw connection
  # value; reverse-engineered this one from reading a
  # single value from an example PEIR file
  con <- as.raw(c(0x1b, 0x0f, 0x58, 0x00))
  num <- read.single(con)
  testthat::expect_equal(num, 8.086942e-39)
})

test_that("read.single multiple value test", {
  # see comment above
  con <- as.raw(c(0x1b, 0x0f, 0x58, 0x00, 0x1b, 0x0f, 0x58, 0x00))
  num <- read.single(con, num = 2)
  testthat::expect_equal(num, c(8.086942e-39, 8.086942e-39))
})

test_that("read.string test", {
  con <- charToRaw("4042")
  str <- read.string(con, len = 4)
  testthat::expect_equal(str, "4042")
})
