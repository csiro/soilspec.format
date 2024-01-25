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

  # return status according to whether the file exists or has non-zero size
  # (1 = does not exist, 4 = zero length, 0 = OK)
  file_status = function(path) {
    if (!file.exists(path)) {
      1
    } else if (file.info(path)$size == 0) {
      4
    } else {
      0
    }
  },

  # each subclass must implement this to return a list
  # of the form returned by create.result
  read = function(path) { list() },

  # create a result list, possibly with some members that are NULL
  create.result = function(status=NULL, mode=NULL, data.df=NULL, meta.list=NULL, std_meta=NULL) {
    result <- list()

print(std_meta)

    result[["status"]] <- status
    result[["mode"]] <- mode
    if (status == 0) {
      result[["is.descending"]] <- data.df[1,]$wavenumber > data.df[nrow(data.df),]$wavenumber
    } else {
      result[["is.descending"]] <- NULL
    }
    result[["origin"]] <- self$origin
    result[["type"]] <- self$type_name
    result[["data"]] <- data.df
    result[["standardisedMetadata"]] <- std_meta
    result[["allInstrumentMetadata"]] <- meta.list

    result
  }

))


