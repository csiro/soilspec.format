# Spectrum format base class

SpectrumFormat <- R6::R6Class("SpectrumFormat", public = list(
  origin = NULL,
  type_name = NULL,
  xunits = NULL,
  yunits = NULL,
  suffix = NULL, # of source spectrum file
  num.rows = NULL, # required in source spectrum file
  # TODO: given XRF, NMR and other non IR spectra, we may need a set of permissible values
  is_reflectance = NULL,

  initialize = function(origin, type_name, method, suffix,
                        xunits = "ARBITRARY UNITS",
                        yunits = "ARBITRARY UNITS",
                        is_reflectance = FALSE) {
    stopifnot(is.character(origin), length(origin) == 1)
    stopifnot(is.character(type_name), length(type_name) == 1)
    stopifnot(is.character(xunits), length(xunits) == 1)
    stopifnot(is.character(yunits), length(yunits) == 1)
    stopifnot(is.character(suffix), length(suffix) == 1)
    stopifnot(is_reflectance == TRUE || is_reflectance == FALSE)

    self$origin <- origin
    self$type_name <- type_name
    self$xunits <- xunits
    self$yunits <- yunits
    self$suffix <- suffix
    self$is_reflectance <- is_reflectance
  },

  create.result = function(data, meta) {
    result <- list()
    result[["data"]] <- data
    result[["metadata"]] <- jsonlite::toJSON(meta)
    result
  }
))
#
# # TODO: move subclasses to separate files
#
# NicoletSpa <- R6::R6Class("NicoletSpa",
#   inherit = SpectrumFormat,
#
#   public = list(
#     initialize = function() {
#       super$initialize(origin = "Nicolet",
#                        type_name = "MIR",
#                        suffix = ".spa",
#                        xunits = "Wavenumber",
#                        yunits = "Reflectance",
#                        is_reflectance = TRUE)
#     },
#
#     read = function(path) {
#       spec.df <- spa2csv$read_spa(path)
#       # TODO: need reticulate and Python code; need filter param?
#       pairs <- spa2strings$key_value_pairs(path, 6, c('?', ';', '$'))
#       meta.df <- data.frame(pairs)
#
#       super$create.result(spec.df, meta.df)
#     }
#   )
# )
#
# BrukerOpusBinary <- R6::R6Class("BrukerOpusBinary",
#   inherit = SpectrumFormat,
#
#   public = list(
#     initialize = function() {
#       super$initialize(origin = "Brucker",
#                        type_name = "MIR",
#                        suffix = ".0",
#                        xunits = "Wavenumber",
#                        yunits = "Reflectance",
#                        is_reflectance = TRUE)
#     },
#
#     read = function(path) {
#       spec.data <- simplerspec::read_opus_bin_univ(path)
#       spec.df <- data.frame(wavenumber=spec.data$wavenumbers, intensity=unlist(spec.data$spc))
#       meta.df <- spec.data$metadata
#       super$create.result(spec.df, meta.df)
#     }
#   )
# )
