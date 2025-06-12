# Generic read type function unit tests
# Tested with Little Endian storage (Intel OK, ARM can be either)
# We test current common usage.

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

test_that("read.double multiple test", {
  con <- writeBin(as.double(c(40.0, 42.0)), raw())
  nums <- read.double(con, num = 2)
  testthat::expect_equal(nums, as.double(c(40.0, 42.0)))
})

# test_that("read.single test", {
#   con <- writeBin(as.single(40.0), raw())
#   num <- read.single(con)
#   testthat::expect_equal(num, as.single(40.0))
# })

test_that("read.string test", {
  con <- charToRaw("4042")
  str <- read.string(con, len = 4)
  testthat::expect_equal(str, "4042")
})
