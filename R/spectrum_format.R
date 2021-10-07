# Spectrum format base class

SpectrumFormat <- R6::R6Class("SpectrumFormat", public = list(
  origin = NULL, # e.g. manufacturer
  type_name = NULL, # e.g. MIR, vis-NIR
  suffix = NULL, # of source spectrum file

  initialize = function(origin, type_name, suffix) {
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
    result[["origin"]] <- self$origin
    result[["type"]] <- self$type_name
    result[["data"]] <- data.df
    result[["metadata"]] <- meta.list

    result
  }
))
