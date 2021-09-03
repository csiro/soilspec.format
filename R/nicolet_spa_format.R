spa2csv <- NULL
spa2strings <- NULL

# TODO: replace dependence upon Python code with a pure R solution

NicoletSpa <- R6::R6Class("NicoletSpa",
  inherit = soilspec.format::SpectrumFormat,
  public = list(
    spa2csv = NULL,
    spa2strings = NULL,

    initialize = function() {
      super$initialize(origin = "Nicolet",
                       type_name = "MIR",
                       suffix = ".spa",
                       xunits = "Wavenumber",
                       yunits = "Reflectance")
    },

    read = function(path) {
      result <- parse_nicolet_spa(path)
      spec.df <- data.frame(wavenumber=result$wavelengths, intensity=result$intensities)
      meta.df <- c()

      super$create.result(spec.df, meta.df)
    }
  )
)
