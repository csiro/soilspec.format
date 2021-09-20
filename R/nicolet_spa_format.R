# Nicolet .spa format, from Nicolet MIR

NicoletSpa <- R6::R6Class("NicoletSpa",
  inherit = soilspec.format::SpectrumFormat,
  public = list(
    initialize = function() {
      super$initialize(origin = "Nicolet MIR",
                       type_name = "MIR",
                       suffix = ".spa")
    },

    read = function(path) {
      result <- parse_nicolet_spa(path)
      status <- as.integer(result$status)

      if (status == 0) {
        spec.df <- data.frame(wavenumber=result$wavelengths, intensity=result$intensities)
        # TODO: implement this!
        meta.df <- data.frame()
        mode <- "absorbance" # TODO!
        units <- "" # TODO! (e.g. cm^-1)
      } else {
        spec.df <- NULL
        meta.df <- NULL
        mode <- NULL
        units <- NULL
      }

      # TODO: mode can be absorbance or reflectance, e.g. see page 81 of
      # https://mmrc.caltech.edu/FTIR/Nicolet/Nicolet%20manuals/Omnic%20User%20Manual%202004.pdf
      # Obtain from metadata?
      super$create.result(status, mode, units, spec.df, meta.df)
    }
  )
)
