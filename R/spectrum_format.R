# Spectrum format base class

# TODO:
# - extraction of units? necessary for standardised transformation?
# - we have is.descending, but standardised forward or reversed wavenumbers needed?
# - standardised metadata: minimum subset? possible, since each different?
# - need a pass-through format for BrukerCSV or indeed, any CSV, but with no metadata
# - check metadata in example data files for possible need to de-identify any

SpectrumFormat <- R6::R6Class("SpectrumFormat", public = list(
  origin = NULL, # e.g. manufacturer
  type_name = NULL, # e.g. MIR, vis-NIR
  suffix = NULL, # of source spectrum file

  initialize = function(origin, type_name, method, suffix) {
    stopifnot(is.character(origin), length(origin) == 1)
    stopifnot(is.character(type_name), length(type_name) == 1)
    stopifnot(is.character(suffix), length(suffix) == 1)

    self$origin <- origin
    self$type_name <- type_name
    self$suffix <- suffix
  },

  # each subclass must implement this
  read = function() {},

  create.result = function(status, mode, units, data.df, meta.list) {
    result <- list()

    result[["status"]] <- status
    result[["mode"]] <- mode
    result[["units"]] <- units
    if (status == 0) {
      result[["is.descending"]] <- data.df[1,]$wavenumber > data.df[nrow(data.df),]$wavenumber
    } else {
      result[["is.descending"]] <- F
    }
    result[["data"]] <- data.df
    result[["metadata"]] <- meta.list

    result
  }
))
