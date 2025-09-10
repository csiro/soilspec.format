# Functions to read a soil spectroscopy file

source("R/spectrum_format.R", local=T)
source("R/nicolet_spa_format.R", local=T)
source("R/thermo_spc_format.R", local=T)
source("R/bruker_opus_binary_format.R", local=T)
source("R/asd_binary_format.R", local=T)
source("R/asd_sco_binary_format.R", local=T)
source("R/perkin_elmer_format.R", local=T)
source("R/csv_format.R", local=T)
source("R/hone_labred_format.R", local=T)
source("R/hone_labred_reduced_format.R",local=T)
source("R/scans_format.R", local=T)
source("R/spectral_evolution_format.R", local=T)

soilspec.readers <- list()
soilspec.readers[[".spa"]] <- NicoletSpa$new()
soilspec.readers[[".spc"]] <- ThermoSpc$new()
soilspec.readers[[".0"]] <- BrukerOpusBinary$new()
soilspec.readers[[".asd"]] <- ASDBinary$new()
soilspec.readers[[".sco"]] <- ASDScoBinary$new()
soilspec.readers[[".sp"]] <- PerkinElmerSP$new()
soilspec.readers[[".csv"]] <- CSV$new()
soilspec.readers[[".hlr"]] <- HoneLabRed$new()
soilspec.readers[[".hlrr"]] <- HoneLabRedReduced$new()
soilspec.readers[[".scan"]] <- SCANS$new()
soilspec.readers[[".sed"]] <- SpectralEvolution$new()

#' Read a spectroscopy file given only its path.
#' @export
#' @param path Full path to the file
#' @return A result list containing a file read status, a data.frame of
#'         wavenumber-intensity pairs, a list of any available metadata,
#'         instrument mode, units, whether wavenumbers are in descending order,
#'         instrument origin, spectrum type;
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
      suffix <- stringr::str_to_lower(suffix)
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
    result[["is.descending"]] <- NULL
    result[["data"]] <- NULL
    result[["metadata"]] <- NULL # TODO: need to be stdmeta etc now?
  }

  result
}

#' Read a Spectral Evolution spectroscopy file given a path.
#' A precondition for correct functioning is that the file is of the expected type.
#' This function should be used when the file does not have the expected ".sed" suffix.
#' @export
#' @param path Full path to the file
#' @return A result list containing a file read status, a data.frame of
#'         wavenumber-intensity pairs, a list of any available metadata,
#'         instrument mode, units, whether wavenumbers are in descending order,
#'         instrument origin, spectrum type;
#'         status will be non-zero if file does not exist or cannot be read (1),
#'         is of an unknown format (2) or some other error occurred (4).
read.spectral.evolution <- function(path) {

  read.soilspec.with.suffix(path, ".sed")
}

#' Read a CSIRO SCANS spectroscopy file given a path.
#' A precondition for correct functioning is that the file is of the expected type.
#' This function should be used when the file does not have the expected ".scan" suffix.
#' @export
#' @param path Full path to the file
#' @return A result list containing a file read status, a data.frame of
#'         wavenumber-intensity pairs, a list of any available metadata,
#'         instrument mode, units, whether wavenumbers are in descending order,
#'         instrument origin, spectrum type;
#'         status will be non-zero if file does not exist or cannot be read (1),
#'         is of an unknown format (2) or some other error occurred (4).
read.csiro.scans <- function(path) {

  read.soilspec.with.suffix(path, ".scan")
}


#' Read a Hone Lab Red spectroscopy file given a path.
#' A precondition for correct functioning is that the file is of the expected type.
#' This function should be used when the file does not have the expected ".hlr" suffix.
#' @export
#' @param path Full path to the file
#' @return A result list containing a file read status, a data.frame of
#'         wavenumber-intensity pairs, a list of any available metadata,
#'         instrument mode, units, whether wavenumbers are in descending order,
#'         instrument origin, spectrum type;
#'         status will be non-zero if file does not exist or cannot be read (1),
#'         is of an unknown format (2) or some other error occurred (4).
read.hone.lab.red <- function(path) {

  read.soilspec.with.suffix(path, ".hlr")
}

#' Read a Reduced Output Hone Lab Red spectroscopy file given a path.
#' A precondition for correct functioning is that the file is of the expected type.
#' This function should be used when the file does not have the expected ".hlrr" suffix.
#' @export
#' @param path Full path to the file
#' @return A result list containing a file read status, a data.frame of
#'         wavenumber-intensity pairs, a list of any available metadata,
#'         instrument mode, units, whether wavenumbers are in descending order,
#'         instrument origin, spectrum type;
#'         status will be non-zero if file does not exist or cannot be read (1),
#'         is of an unknown format (2) or some other error occurred (4).
read.hone.lab.red.reduced <- function(path) {

  read.soilspec.with.suffix(path, ".hlrr")
}


#' Read a Nicolet spa spectroscopy file given a path.
#' A precondition for correct functioning is that the file is of the expected type.
#' This function should be used when the file does not have the expected ".spa" suffix.
#' @export
#' @param path Full path to the file
#' @return A result list containing a file read status, a data.frame of
#'         wavenumber-intensity pairs, a list of any available metadata,
#'         instrument mode, units, whether wavenumbers are in descending order,
#'         instrument origin, spectrum type;
#'         status will be non-zero if file does not exist or cannot be read (1),
#'         is of an unknown format (2) or some other error occurred (4).
read.nicolet.spa <- function(path) {

  read.soilspec.with.suffix(path, ".spa")
}

#' Read a Bruker Opus Binary spectroscopy file.
#' This function should be used when the file does not have the expected ".0" suffix.
#' A precondition for correct functioning is that the file is of the expected type.
#' @export
#' @param path Full path to the file
#' @return A result list containing a file read status, a data.frame of
#'         wavenumber-intensity pairs, a list of any available metadata,
#'         instrument mode, units, whether wavenumbers are in descending order,
#'         instrument origin, spectrum type;
#'         status will be non-zero if file does not exist or cannot be read (1),
#'         is of an unknown format (2) or some other error occurred (4).
read.bruker.opus.binary <- function(path) {

  read.soilspec.with.suffix(path, ".0")
}

#' Read a Perkin Elmer spectroscopy file.
#' This function should be used when the file does not have the expected ".sp" suffix.
#' A precondition for correct functioning is that the file is of the expected type.
#' @export
#' @param path Full path to the file
#' @return A result list containing a file read status, a data.frame of
#'         wavenumber-intensity pairs, a list of any available metadata,
#'         instrument mode, units, whether wavenumbers are in descending order,
#'         instrument origin, spectrum type;
#'         status will be non-zero if file does not exist or cannot be read (1),
#'         is of an unknown format (2) or some other error occurred (4).
read.perkin.elmer.sp <- function(path) {

  read.soilspec.with.suffix(path, ".sp")
}

#' Read a CSV spectroscopy file.
#' This function should be used when the file does not have the expected ".csv" suffix.
#' A precondition for correct functioning is that the file is of the expected type.
#' @export
#' @param path Full path to the file
#' @param is.absorbance Does the sample data correspond to absorbance?
#' @param is.reflectance Does the sample data correspond to reflectance?
#' @param is.transmittance Does the sample data correspond to transmittance?
#' @param source.col.names Vector of CSV column names to be read corresponding
#'        to wavenumber and intensity data frame columns.
#' @return A result list containing a file read status, a data.frame of
#'         wavenumber-intensity pairs, a list of any available metadata,
#'         instrument mode, units, whether wavenumbers are in descending order,
#'         instrument origin, spectrum type;
#'         status will be non-zero if file does not exist or cannot be read (1),
#'         is of an unknown format (2) or some other error occurred (4).
read.soilspec.csv <- function(path,
                              is.absorbance = F, is.reflectance = F, is.transmittance = F,
                              source.col.names = c("wavenumber", "intensity")) {

  read.soilspec.with.suffix(path, ".csv", is.absorbance, is.reflectance, is.transmittance,
                            source.col.names)
}

#' Read an ASD Binary spectroscopy file.
#' This function should be used when the file does not have the expected ".asd" suffix.
#' A precondition for correct functioning is that the file is of the expected type.
#' @export
#' @param path Full path to the file
#' @return A result list containing a file read status, a data.frame of
#'         wavenumber-intensity pairs, a list of any available metadata,
#'         instrument mode, units, whether wavenumbers are in descending order,
#'         instrument origin, spectrum type;
#'         status will be non-zero if file does not exist or cannot be read (1),
#'         is of an unknown format (2) or some other error occurred (4).
read.asd.binary <- function(path) {

  read.soilspec.with.suffix(path, ".asd")
}

#' Read an ASD SCO Binary spectroscopy file.
#' This function should be used when the file does not have the expected ".sco" suffix.
#' A precondition for correct functioning is that the file is of the expected type.
#' @export
#' @param path Full path to the file
#' @return A result list containing a file read status, a data.frame of
#'         wavenumber-intensity pairs, a list of any available metadata,
#'         instrument mode, units, whether wavenumbers are in descending order,
#'         instrument origin, spectrum type;
#'         status will be non-zero if file does not exist or cannot be read (1),
#'         is of an unknown format (2) or some other error occurred (4).
read.asd.sco.binary <- function(path) {

  read.soilspec.with.suffix(path, ".sco")
}

#' Read a Thermo spc spectroscopy file.
#' This function should be used when the file does not have the expected ".spc" suffix.
#' A precondition for correct functioning is that the file is of the expected type.
#' @export
#' @param path Full path to the file
#' @return A result list containing a file read status, a data.frame of
#'         wavenumber-intensity pairs, a list of any available metadata,
#'         instrument mode, units, whether wavenumbers are in descending order,
#'         instrument origin, spectrum type;
#'         status will be non-zero if file does not exist or cannot be read (1),
#'         is of an unknown format (2) or some other error occurred (4).
read.thermo.spc <- function(path) {

  read.soilspec.with.suffix(path, ".spc")
}


#' Spectroscopy file reader function with assumed suffix explicitly supplied.
#' This function should be used when the file does not have the expected suffix.
#' A precondition for correct functioning is that the file and assumed suffix are compatible.
#' @param path Full path to the file
#' @param assumed.suffix The assumed suffix for the expected file format, e.g. .spa, .0
#' @param is.absorbance Does the sample data correspond to absorbance?
#' @param is.reflectance Does the sample data correspond to reflectance?
#' @param is.transmittance Does the sample data correspond to transmittance?
#' @return A result list containing a file read status, a data.frame of
#'         wavenumber-intensity pairs, a list of any available metadata,
#'         instrument mode, units, whether wavenumbers are in descending order,
#'         instrument origin, spectrum type;
#'         status will be non-zero if file does not exist or cannot be read (1),
#'         is of an unknown format (2) or some other error occurred (4).
read.soilspec.with.suffix <- function(path, assumed.suffix,
                                      is.absorbance = F, is.reflectance = F, is.transmittance = F,
                                      source.col.names = c("wavenumber", "intensity")) {

  result <- NULL

  if (!file.exists(path)) {
    status <- 1
  } else {
    assumed.suffix <- stringr::str_to_lower(assumed.suffix)
    if (assumed.suffix %in% names(soilspec.readers)) {
      if (assumed.suffix == ".csv") {
        result <- soilspec.readers[[assumed.suffix]]$read(path,
                                                          is.absorbance, is.reflectance, is.transmittance,
                                                          source.col.names)
      } else {
        result <- soilspec.readers[[assumed.suffix]]$read(path)
      }
    } else {
      status <- 2
    }
  }

  if (is.null(result)) {
    result <- list()
    result[["status"]] <- status
    result[["mode"]] <- NULL
    result[["is.descending"]] <- NULL
    result[["data"]] <- NULL
    result[["metadata"]] <- NULL
  }

  result
}
