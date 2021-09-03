# Spectrum format base class

SpectrumFormat <- R6::R6Class("SpectrumFormat", public = list(
  origin = NULL,
  type_name = NULL,
  xunits = NULL,
  yunits = NULL,
  suffix = NULL, # of source spectrum file
  num.rows = NULL, # required in source spectrum file

  initialize = function(origin, type_name, method, suffix,
                        xunits = "ARBITRARY UNITS",
                        yunits = "ARBITRARY UNITS") {
    stopifnot(is.character(origin), length(origin) == 1)
    stopifnot(is.character(type_name), length(type_name) == 1)
    stopifnot(is.character(xunits), length(xunits) == 1)
    stopifnot(is.character(yunits), length(yunits) == 1)
    stopifnot(is.character(suffix), length(suffix) == 1)

    self$origin <- origin
    self$type_name <- type_name
    self$xunits <- xunits
    self$yunits <- yunits
    self$suffix <- suffix
  },

  create.result = function(data, meta) {
    result <- list()
    result[["data"]] <- data
    result[["metadata"]] <- jsonlite::toJSON(meta)
    result
  }
))
