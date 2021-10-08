# Bruker Opus binary format, from Bruker MIR

# TODO: obtain mode per file

BrukerOpusBinary <- R6::R6Class("BrukerOpusBinary",
  inherit = SpectrumFormat,

  public = list(
    initialize = function() {
      super$initialize(origin = "Bruker",
                       type_name = "MIR",
                       suffix = ".0")
    },

    read = function(path) {
      spec.df <- NULL
      meta.list <- NULL
      status <- 4
      mode <- NULL
      units <- NULL

      out <- tryCatch({
        spec.data <- opusreader::opus_read(path)
        mode <- "absorbance" # TODO: always absorbance? see help doc re: extract parameter
        units <- "?" # TODO: (e.g. cm^-1)
        spec.df <- data.frame(wavenumber=spec.data$wavenumbers, intensity=c(spec.data$spec))
        meta.list <- as.list(spec.data$metadata)
        status <- 0
      },
      error=function(cond) {
      },
      warning=function(cond) {
      },
      finally={
      })

      super$create.result(status, mode, units, spec.df, meta.list)
    }
  )
)
