# Functions to read a soil spectroscopy file

source("R/spectrum_format.R", local=T)
source("R/nicolet_spa_format.R", local=T)
source("R/thermo_spc_format.R", local=T)
source("R/bruker_opus_binary_format.R", local=T)
source("R/asd_binary_format.R", local=T)

soilspec.readers <- list()
soilspec.readers[[".spa"]] <- NicoletSpa$new()
soilspec.readers[[".spc"]] <- ThermoSpc$new()
soilspec.readers[[".0"]] <- BrukerOpusBinary$new()
soilspec.readers[[".asd"]] <- ASDBinary$new()

#' Read a soil spectroscopy file given only its path.
#' @export
#' @param path Full path to the file
#' @return A result list containing a file read status, a data.frame of
#'         wavenumber-intensity pairs, a list of any available metadata,
#'         instrument mode, units, whether wavenumbers are in descending order;
#'         status will be non-zero if file does not exist or cannot be read (1),
#'         is of an unknown format (2) or some other error occurred (4)
read.soilspec <- function(path) {

  result <- NULL

  if (!file.exists(path)) {
    status <- 1
  } else {
    fields <- stringr::str_split(path, pattern="\\.")
    if (length(fields[[1]]) > 1) {
      suffix <- paste0(".", fields[[1]][length(fields[[1]])])
      if (suffix %in% names(soilspec.readers)) {
        result <- soilspec.readers[[suffix]]$read(path)
      } else {
        status <- 2
      }
    } else {
      status <- 2
    }
  }

  if (is.null(result)) {
    result <- list()
    result[["status"]] <- status
    result[["mode"]] <- NULL
    result[["units"]] <- NULL
    result[["is.descending"]] <- NULL
    result[["data"]] <- NULL
    result[["metadata"]] <- NULL
  }

  result
}

#' Read a Nicolet spa soil spectroscopy file given a path.
#' A precondition for correct functioning is that the file is of the expected type.
#' This function should be used when the file does not have the expected ".spa" suffix.
#' @export
#' @param path Full path to the file
#' @return A result list containing a file read status, a data.frame of
#'         wavenumber-intensity pairs, a list of any available metadata,
#'         instrument mode, units, whether wavenumbers are in descending order;
#'         status will be non-zero if file does not exist or cannot be read (1),
#'         is of an unknown format (2) or some other error occurred (4).
read.nicolet.spa <- function(path) {

  read.soilspec.common(path, ".spa")
}

#' Read a Bruker Opus Binary soil spectroscopy file.
#' This function should be used when the file does not have the expected ".0" suffix.
#' A precondition for correct functioning is that the file is of the expected type.
#' @export
#' @param path Full path to the file
#' @return A result list containing a file read status, a data.frame of
#'         wavenumber-intensity pairs, a list of any available metadata,
#'         instrument mode, units, whether wavenumbers are in descending order;
#'         status will be non-zero if file does not exist or cannot be read (1),
#'         is of an unknown format (2) or some other error occurred (4).
read.bruker.opus.binary <- function(path) {

  read.soilspec.common(path, ".0")
}

#' Read an ASD Binary soil spectroscopy file.
#' This function should be used when the file does not have the expected ".asd" suffix.
#' A precondition for correct functioning is that the file is of the expected type.
#' @export
#' @param path Full path to the file
#' @return A result list containing a file read status, a data.frame of
#'         wavenumber-intensity pairs, a list of any available metadata,
#'         instrument mode, units, whether wavenumbers are in descending order;
#'         status will be non-zero if file does not exist or cannot be read (1),
#'         is of an unknown format (2) or some other error occurred (4).
read.asd.binary <- function(path) {

  read.soilspec.common(path, ".asd")
}

#' Read a Thermo spc soil spectroscopy file.
#' This function should be used when the file does not have the expected ".spc" suffix.
#' A precondition for correct functioning is that the file is of the expected type.
#' @export
#' @param path Full path to the file
#' @return A result list containing a file read status, a data.frame of
#'         wavenumber-intensity pairs, a list of any available metadata,
#'         instrument mode, units, whether wavenumbers are in descending order;
#'         status will be non-zero if file does not exist or cannot be read (1),
#'         is of an unknown format (2) or some other error occurred (4).
read.thermo.spc <- function(path) {

  read.soilspec.common(path, ".spc")
}


#' Common soil spectroscopy file reader function.
#' This function should be used when the file does not have the expected suffix.
#' A precondition for correct functioning is that the file and assumed suffix are compatible.
#' @export
#' @param path Full path to the file
#' @param assumed.suffix The assumed suffix for the expected file format
#' @return A result list containing a file read status, a data.frame of
#'         wavenumber-intensity pairs, a list of any available metadata,
#'         instrument mode, units, whether wavenumbers are in descending order;
#'         status will be non-zero if file does not exist or cannot be read (1),
#'         is of an unknown format (2) or some other error occurred (4).
read.soilspec.common <- function(path, assumed.suffix) {

  result <- NULL

  if (!file.exists(path)) {
    status <- 1
  } else {
    if (assumed.suffix %in% names(soilspec.readers)) {
      result <- soilspec.readers[[assumed.suffix]]$read(path)
    } else {
      status <- 2
    }
  }

  if (is.null(result)) {
    result <- list()
    result[["status"]] <- status
    result[["mode"]] <- NULL
    result[["units"]] <- NULL
    result[["is.descending"]] <- NULL
    result[["data"]] <- NULL
    result[["metadata"]] <- NULL
  }

  result
}
