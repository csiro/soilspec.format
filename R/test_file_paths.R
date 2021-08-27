# Example file paths

#' Return the full path to the Bruker Opus Binary test file
#' @export
#' @return full path to the Bruker Opus Binary test file
bruker.opus.binary.file.path <- function() {
  system.file("extdata", "BrukerOpusBinary", "example.0",
              package = "soilspec.format")
}
