# Example file path access functions

#' Return the full path to the Bruker Opus Binary example file
#' @export
#' @return The full path to the Bruker Opus Binary example file
bruker.opus.binary.file.path <- function() {
  system.file("extdata", "BrukerOpusBinary", "example.0",
              package = "soilspec.format")
}

#' Return the full path to the (Bruker) CSV example file
#' @export
#' @return The full path to the (Bruker) CSV example file
csv.file.path <- function() {
  system.file("extdata", "BrukerCSV", "example.csv",
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

#' Return the full path to the Thermo spc example file
#' @export
#' @return The full path to the Thermo spc example file
thermo.spc.file.path <- function() {
  system.file("extdata", "ThermoSpc", "example.spc",
              package = "soilspec.format")
}

#' Return the full path to the Unknown example file (for testing purposes)
#' @export
#' @return The full path to the Unknown example file
unknown.file.path <- function() {
  system.file("extdata", "Unknown", "example.xyz",
              package = "soilspec.format")
}

