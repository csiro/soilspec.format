# Spectrum format base class

# TODO:
# - extraction of units? necessary for standardised transformation?
# - we have is.descending, but need standardised forward or reversed wavenumbers?
# - standardised metadata: minimum subset? possible, since each different?
# - create a list of suffixes to handlers and a top-level export function that
#   uses this (if (suffix %in% list), with helper functions that get singleton object by suffix
# - need a pass-through format for BrukerCSV or indeed, any CSV, but with no metadata

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

  create.result = function(status, mode, units, data.df, meta.list) {
    result <- list()

    result[["status"]] <- status
    result[["mode"]] <- mode
    result[["units"]] <- units
    result[["is.descending"]] <- data.df[1,]$wavenumber > data.df[nrow(data.df),]$wavenumber
    result[["data"]] <- data.df
    result[["metadata"]] <- meta.list

    result
  }
))
