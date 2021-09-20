# Spectrum format base class
# TODO:
# - do we want to return metadata as lists, data frames?
# - enumeration of units
# - forward or reversed wavenumbers
# - missing or less rich metadata in some formats (e.g. Nicolet)
# - standardising metadata: possible? useful as-is since each different?

SpectrumFormat <- R6::R6Class("SpectrumFormat", public = list(
  origin = NULL,
  type_name = NULL,
  suffix = NULL, # of source spectrum file

  initialize = function(origin, type_name, method, suffix) {
    stopifnot(is.character(origin), length(origin) == 1)
    stopifnot(is.character(type_name), length(type_name) == 1)
    stopifnot(is.character(suffix), length(suffix) == 1)

    self$origin <- origin
    self$type_name <- type_name
    self$suffix <- suffix
  },

  create.result = function(status, mode, units, data.df, metadata.df) {
    result <- list()

    result[["status"]] <- status
    result[["mode"]] <- mode
    result[["units"]] <- units
    result[["data"]] <- data.df
    result[["metadata"]] <- metadata.df

    result
  }
))
