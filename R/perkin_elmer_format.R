# Perkin Elmer spectra formats
#
# We handle two formats:
# - PEPE in first 4 bytes of file (block structured)
# - PE IR in first 5 bytes of file
#
# Re: PEPE, see: https://au.mathworks.com/matlabcentral/fileexchange/22736-perkinelmer-ir-data-file-import-tools?s_tid=srchtitle
# PE IR format has so far only been understood by inspection.
# Official Perkin Elmer format specifications would be helpful though.

PerkinElmerSP <- R6::R6Class("PerkinElmerSP",
  inherit = SpectrumFormat,
  public = list(
    initialize = function() {
      super$initialize(origin = "Perkin Elmer",
                       type_name = "IR",
                       suffix = ".sp")
    },

    read = function(path) {
      if (!file.exists(path)) {
        status <- 1
      } else {
        sp.file <- file(path, "rb")
        signature <- read.string(sp.file, 5)
        close(sp.file)

        if (stringr::str_sub(signature, 1, 4) == "PEPE") {
          result <- read.pepe(path)
          status <- result$status
        } else if (stringr::str_sub(signature, 1, 5) == "PE IR") {
          # TODO
          status <- 2
        } else {
          status <- 2
        }
      }

      if (status == 0) {
        spec.df <- data.frame(wavenumber=result$x, intensity=result$y)
        meta.list <- result$metadata
        mode <- NULL # appears to be present in the data though
      } else {
        status <- 4
        spec.df <- NULL
        meta.list <- NULL
        mode <- NULL
      }

      super$create.result(status, mode, spec.df, meta.list)
    }
  )
)

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

read.string <- function(con, len, num=1) {
  result <- readBin(con, "raw", len)
  if (length(result) == 0) {
    result <- NA
  } else {
    result <- rawToChar(result)
  }
  result
}

read.pepe <- function(path) {
  # block IDs
  # there are block IDs we are not processing here
  DSet2DC1DIBlock <- 120
  DataSetAbscissaRangeMember <- -29838
  DataSetIntervalMember <- -29836
  DataSetNumPointsMember <- -29835
  DataSetXAxisLabelMember <- -29833
  DataSetYAxisLabelMember <- -29832
  DataSetAliasMember <- -29823
  DataSetNameMember <- -29827
  DataSetDataMember <- -29828

  sp.file <- file(path, "rb")

  description <- read.string(sp.file, 38)
  # skip remaining description bytes that yield error
  seek(sp.file, where = 6, origin = "current")

  xLen <- as.integer(0)
  original.name <- NULL
  at.end <- F

  while (!at.end) {
    block.id <- read.int16(sp.file)
    block.size <- read.int32(sp.file)

    at.end <- is.na(block.id) || is.na(block.size)

    if (!at.end) {
      if (block.id == DSet2DC1DIBlock) {
        # wrapper block; nothing to do
      } else if (block.id == DataSetAbscissaRangeMember) {
        inner.code <- read.int16(sp.file)
        x0 <- read.double(sp.file)
        xEnd <- read.double(sp.file)
      } else if (block.id == DataSetIntervalMember) {
        inner.code <- read.int16(sp.file)
        xDelta <- read.double(sp.file)
      } else if (block.id == DataSetNumPointsMember) {
        inner.code <- read.int16(sp.file)
        xLen <- read.int32(sp.file)
      } else if (block.id == DataSetXAxisLabelMember) {
        inner.code <- read.int16(sp.file)
        len <- read.int16(sp.file)
        xLabel <- read.string(sp.file, len)
      } else if (block.id == DataSetYAxisLabelMember) {
        inner.code <- read.int16(sp.file)
        len <- read.int16(sp.file)
        yLabel <- read.string(sp.file, len)
      } else if (block.id == DataSetAliasMember) {
        inner.code <- read.int16(sp.file)
        len <- read.int16(sp.file)
        alias <- read.string(sp.file, len)
      } else if (block.id == DataSetNameMember) {
        inner.code <- read.int16(sp.file)
        len <- read.int16(sp.file)
        original.name <- read.string(sp.file, len)
      }
      else if (block.id == DataSetDataMember) {
        inner.code <- read.int16(sp.file)
        len <- read.int32(sp.file)
        # len should be xLen * 8
        if (xLen == 0) {
          xLen <- as.integer(len / 8)
        }
        data <- read.double(sp.file, num = xLen)
      } else {
        # unknown block; just seek past it
        seek(sp.file, where = block.size, origin = "current")
      }
    }
  }

  close(sp.file)

  result <- list()

  if (xLen == 0) {
    status <- 2
  } else {
    status <- 0

    # create a sequence from the x-axis specification
    result[["x"]] <- seq(x0, xEnd, xDelta)

    result[["y"]] <- data

    # return metadata as name-value pairs
    metadata <- list()
    metadata[["description"]] <- description
    metadata[["xLabel"]] <- xLabel
    metadata[["yLabel"]] <- yLabel
    metadata[["alias"]] <- alias
    metadata[["original name"]] <- original.name
    result[["metadata"]] <- metadata

    result[["status"]] <- status
  }

  result
}
