#source("spectrum_format.R")

BrukerOpusBinary <- R6::R6Class("BrukerOpusBinary",
  inherit = SpectrumFormat,

  public = list(
    initialize = function() {
      super$initialize(origin = "Brucker",
                       type_name = "MIR",
                       suffix = ".0",
                       xunits = "Wavenumber",
                       yunits = "Reflectance",
                       is_reflectance = TRUE)
    },

    read = function(path) {
      spec.data <- simplerspec::read_opus_bin_univ(path)
      spec.df <- data.frame(wavenumber=spec.data$wavenumbers, intensity=unlist(spec.data$spc))
      meta.df <- spec.data$metadata
      super$create.result(spec.df, meta.df)
    }
  )
)
