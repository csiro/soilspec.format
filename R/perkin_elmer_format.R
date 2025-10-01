# Perkin Elmer spectra formats
#
# We handle two formats:
# - PEPE in first 4 bytes of file (block structured)
# - PE IR in first 5 bytes of file
#
# For PEPE, see https://au.mathworks.com/matlabcentral/fileexchange/22736-perkinelmer-ir-data-file-import-tools?s_tid=srchtitle
# PE IR format has been understood by inspection.

source("R/read_type.R", local=TRUE)

PerkinElmerSP <- R6::R6Class("PerkinElmerSP",
  inherit = SpectrumFormat,
  public = list(
    initialize = function() {
      super$initialize(origin = "Perkin Elmer",
                       type_name = "IR",
                       suffix = ".sp")
    },

    create.standard.metadata.container = function(meta.list) {
      stdmeta <- super$create.standard.metadata.container()
      stdmeta[['spectra_wavesignature_units']] <- 'wn'

      if (!is.null(meta.list$alias)) {
        # PEPE has alias, PEIR has name but unclear whether name is identifier
        # or path
        suffix_index <- stringr::str_locate(meta.list$alias, pattern='.sp')
        sample_id <- stringr::str_sub(meta.list$alias, 1, suffix_index-1)[1]
        stdmeta$sample_id <- sample_id
      }

      stdmeta
    },

    read = function(path) {
      status <- super$file_status(path)

      if (status == 0) {
        # which Perkin Elmer format is it?
        sp.file <- file(path, "rb")
        signature <- read.string(sp.file, 5)
        close(sp.file)

        if (is.na(signature)) {
          status <- 4
        } else {
          if (stringr::str_sub(signature, 1, 4) == "PEPE") {
            result <- read.pepe(path)
            status <- result$status
          } else if (stringr::str_sub(signature, 1, 5) == "PE IR") {
            result <- read.peir(path)
            status <- result$status
          } else {
            status <- 2
          }
        }
      }

      if (status == 0) {
        spec.df <- result$data
        meta.list <- result$metadata

        stdmeta <- self$create.standard.metadata.container(meta.list)

        mode <- result$metadata$mode
      } else {
        spec.df <- NULL
        stdmeta <- NULL
        meta.list <- NULL
        mode <- NULL
      }

      super$create.result(status, mode, spec.df, meta.list, std_meta=stdmeta)
    }
  )
)

# This function is a partial port of the spload() function found here:
# https://au.mathworks.com/matlabcentral/fileexchange/22736-perkinelmer-ir-data-file-import-tools
#
# Citation: Ben Perston (2025). PerkinElmer IR data file import tools
# (https://www.mathworks.com/matlabcentral/fileexchange/22736-perkinelmer-ir-data-file-import-tools),
# MATLAB Central File Exchange. Retrieved September 29, 2025.
#
# In spload() function header comment:
# Copyright (C)2007 PerkinElmer Life and Analytical Sciences
# Stephen Westlake, Seer Green

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

    # data frame with sequence from x-axis specification, and y data
    result[["data"]] <- data.frame(wavenumber=seq(x0, xEnd, xDelta), intensity=data)

    # return metadata as name-value pairs
    metadata <- list()
    metadata[["description"]] <- description
    metadata[["xLabel"]] <- xLabel
    metadata[["yLabel"]] <- yLabel
    metadata[["alias"]] <- alias
    metadata[["mode"]] <- NULL # see issue #25
    metadata[["original name"]] <- original.name
    result[["metadata"]] <- metadata
  }

  result[["status"]] <- status

  result
}

scale.intensity <- function(intensity, intensity.min, intensity.max, range.min, range.max) {
  # for one reasonable approach, see:
  #  https://stackoverflow.com/questions/5294955/how-to-scale-down-a-range-of-numbers-with-a-known-min-and-max-value
  (range.max - range.min)*(intensity - intensity.min) / (intensity.max - intensity.min) + range.min
}

read.peir <- function(path) {
  # read metadata
  lines <- readLines(path, n = 56)

  if (length(lines) != 56) {
    status <- 2
  } else {
    description <- stringr::str_trim(lines[1])
    name <- stringr::str_trim(lines[3])
    software <- stringr::str_trim(lines[21])
    mode <- stringr::str_trim(lines[33])
    type <- stringr::str_trim(lines[23])
    xLabel <- stringr::str_trim(lines[46])
    yLabel <- stringr::str_trim(lines[47])
    x0 <- as.integer(lines[50])
    xEnd <- as.integer(lines[10])
    xDelta <- as.integer(lines[51])
    datum.count <- as.integer(lines[52])
    range.max <- as.double(lines[54])
    range.min <- as.double(lines[55])

    result <- list()

    if (datum.count == 0) {
      status <- 2
    } else {
      status <- 0
      # read data
      # see issue #24
      sp.in <- file(path, "rb")
      seek(sp.in, 32*15+16, "start")
      data <- read.single(sp.in, datum.count)
      close(sp.in)

      scaled.data <- lapply(data,
                            function(n) {scale.intensity(n, min(data), max(data), range.min, range.max)})

      # data frame with sequence from x-axis specification, and y data
      result[["data"]] <- data.frame(wavenumber=seq(x0, xEnd, xDelta), intensity=unlist(scaled.data))

      # return metadata as name-value pairs
      metadata <- list()
      metadata[["description"]] <- description
      metadata[["software"]] <- software
      metadata[["xLabel"]] <- xLabel
      metadata[["yLabel"]] <- yLabel
      metadata[["mode"]] <- mode
      metadata[["type"]] <- type
      metadata[["name"]] <- name
      result[["metadata"]] <- metadata
    }
  }

  result[["status"]] <- status

  result
}
