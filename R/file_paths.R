# Example and test file path access functions

#' Return the full path to the Spectral Evolution example file
#' @export
#' @return The full path to the Spectral Evolution example file
spectral.evolution.file.path <- function() {
  system.file("extdata", "SpectralEvolution", "example.sed", package = "soilspec.format")
}

#' Return the full path to the CSIRO SCANS example file
#' @export
#' @return The full path to the CSIRO SCANS example file
csiro.scans.file.path <- function() {
  system.file("extdata", "CSIROSCANS", "example.scan", package = "soilspec.format")
}

#' Return the full path to the Hone Lab Red example file
#' @export
#' @return The full path to the Hone Lab Red example file
hone.lab.red.file.path <- function() {
  system.file("extdata", "HoneLabRed", "example.hlr", package = "soilspec.format")
}

#' Return the full path to the Hone Lab Red Reduced example file
#' @export
#' @return The full path to the Hone Lab Red Reduced example file
hone.lab.red.reduced.file.path <- function() {
  system.file("extdata", "HoneLabRedReduced", "31462.hlrr", package = "soilspec.format")
}

#' Return the full path to the Bruker Opus Binary example file
#' @export
#' @return The full path to the Bruker Opus Binary example file
bruker.opus.binary.file.path <- function() {
  system.file("extdata", "BrukerOpusBinary", "example.0",
              package = "soilspec.format")
}

#' Return the full path to the Bruker Opus Binary reflection test file
#' @export
#' @return The full path to the Bruker Opus Binary reflection test file
bruker.opus.binary.test.refl.file.path <- function() {
  system.file("extdata", "BrukerOpusBinary", "test_refl.0",
              package = "soilspec.format")
}

#' Return the full path to the Bruker Opus Binary absorption test file
#' @export
#' @return The full path to the Bruker Opus Binary absorption test file
bruker.opus.binary.test.abs.file.path <- function() {
  system.file("extdata", "BrukerOpusBinary", "test_abs.0",
              package = "soilspec.format")
}

#' Return the full path to the CSV example file
#' @export
#' @return The full path to the CSV example file
csv.file.path <- function() {
  system.file("extdata", "CSV", "example.csv",
              package = "soilspec.format")
}

#' Return the full path to the CSV test file
#' @export
#' @return The full path to the CSV test file
csv.test.file.path <- function() {
  system.file("extdata", "CSV", "test.csv",
              package = "soilspec.format")
}

#' Return the full path to the Nicolet spa example file
#' @export
#' @return The full path to the Nicolet spa example file
nicolet.spa.file.path <- function() {
  system.file("extdata", "NicoletSpa", "example.spa",
              package = "soilspec.format")
}

#' Return the full path to the ASD binary example file
#' @export
#' @return The full path to the ASD binary example file
asd.binary.file.path <- function() {
  system.file("extdata", "ASDBinary", "example.asd",
              package = "soilspec.format")
}

#' Return the full path to the ASD SCO binary example file
#' @export
#' @return The full path to the ASD SCO binary example file
asd.sco.binary.file.path <- function() {
  system.file("extdata", "ASDScoBinary", "example.asd.sco",
              package = "soilspec.format")
}

#' Return the full path to the Thermo spc example file
#' @export
#' @return The full path to the Thermo spc example file
thermo.spc.file.path <- function() {
  system.file("extdata", "ThermoSpc", "example.spc",
              package = "soilspec.format")
}

#' Return the full path to the Perkin Elmer PEPE example file
#' @export
#' @return The full path to the Perkin Elmer PEPE example file
perkin.elmer.sp.pepe.file.path <- function() {
  system.file("extdata", "PerkinElmerPEPE", "example.sp",
              package = "soilspec.format")
}

#' Return the full path to the Perkin Elmer PE IR example file
#' @export
#' @return The full path to the Perkin Elmer PE IR example file
perkin.elmer.sp.peir.file.path <- function() {
  system.file("extdata", "PerkinElmerPEIR", "example.sp",
              package = "soilspec.format")
}

#' Return the full path to the Unknown example file (for testing purposes)
#' @export
#' @return The full path to the Unknown example file
unknown.file.path <- function() {
  system.file("extdata", "Unknown", "example.xyz",
              package = "soilspec.format")
}
