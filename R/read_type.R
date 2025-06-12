# Generic type-specific functions

read.int16 <- function(con, num=1, signed=T) {
  result <- readBin(con, "integer", signed = signed, size = 2, n = num)
  if (length(result) == 0) {
    result <- NA
  }
  result
}

read.int32 <- function(con, num=1, signed=T) {
  result <- readBin(con, "integer", signed = signed, size = 4, n = num)
  if (length(result) == 0) {
    result <- NA
  }
  result
}

read.double <- function(con, num=1) {
  result <- readBin(con, "double", n = num)
  if (length(result) == 0) {
    result <- NA
  }
  result
}

read.single <- function(con, num=1) {
  result <- readBin(con, "numeric", size = 4, n = num)
  if (length(result) == 0) {
    result <- NA
  }
  result
}

read.string <- function(con, len) {
  result <- readBin(con, "raw", len)
  if (length(result) == 0) {
    result <- NA
  } else {
    result <- rawToChar(result)
  }
  result
}
