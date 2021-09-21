# Bruker Opus binary format, from Bruker MIR

BrukerOpusBinary <- R6::R6Class("BrukerOpusBinary",
  inherit = soilspec.format::SpectrumFormat,

  public = list(
    initialize = function() {
      super$initialize(origin = "Bruker MIR",
                       type_name = "MIR",
                       suffix = ".0")
    },

    read = function(path) {
      spec.df <- NULL
      meta.df <- NULL
      status <- 1
      mode <- NULL
      units <- NULL

      out <- tryCatch({
        spec.data <- simplerspec::read_opus_bin_univ(path)
        mode <- "absorbance" # TODO: obtain from metadata?
        units <- "" # TODO: (e.g. cm^-1)
        spec.df <- data.frame(wavenumber=spec.data$wavenumbers, intensity=unlist(spec.data$spc))
        meta.df <- as.data.frame(spec.data$metadata)
        status <- 0
      },
      error=function(cond) {
      },
      warning=function(cond) {
      },
      finally={
      })

      super$create.result(status, mode, units, spec.df, meta.df)
    }
  )
)
