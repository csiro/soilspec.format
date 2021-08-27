#source("spectrum_format.R")

NicoletSpa <- R6::R6Class("NicoletSpa",
  inherit = SpectrumFormat,

  public = list(
    initialize = function() {
      super$initialize(origin = "Nicolet",
                       type_name = "MIR",
                       suffix = ".spa",
                       xunits = "Wavenumber",
                       yunits = "Reflectance",
                       is_reflectance = TRUE)
    },

    read = function(path) {
      spec.df <- spa2csv$read_spa(path)
      # TODO: need reticulate and Python code; need filter param?
      pairs <- spa2strings$key_value_pairs(path, 6, c('?', ';', '$'))
      meta.df <- data.frame(pairs)

      super$create.result(spec.df, meta.df)
    }
  )
)
