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
  # (1 = does not exist, 2 = zero length (so, no suitable format), 0 = OK)
  file_status = function(path) {
    if (!file.exists(path)) {
      1
    } else if (file.info(path)$size == 0) {
      2
    } else {
      0
    }
  },

  # each subclass must implement this to return a list
  # of the form returned by create.result
  read = function(path) { list() },

  determine.mode = function(mode) {
    mode.bool <- list()

    mode.bool[["is.absorbance"]] <- FALSE
    mode.bool[["is.reflectance"]] <- FALSE
    mode.bool[["is.transmittance"]] <- FALSE

    if (length(mode) != 0) {
      mode.lower <- stringr::str_to_lower(mode)

      if (stringr::str_detect(mode.lower, "ab")) {
        mode.bool[["is.absorbance"]] <- TRUE
      } else if (stringr::str_detect(mode.lower, "rfl")) {
        mode.bool[["is.reflectance"]] <- TRUE
      } else if (stringr::str_detect(mode.lower, "refl")) {
        mode.bool[["is.reflectance"]] <- TRUE
      } else if (stringr::str_detect(mode.lower, "tran")) {
        # Note: we will need example transmittance files to test this
        mode.bool[["is.transmittance"]] <- TRUE
      }
    }

    # at most one mode Boolean can be true!
    all.false <- all(!mode.bool[["is.absorbance"]],
                     !mode.bool[["is.reflectance"]],
                     !mode.bool[["is.transmittance"]])

    one.true <- xor(mode.bool[["is.absorbance"]],
                    xor(mode.bool[["is.reflectance"]],
                        mode.bool[["is.transmittance"]]))

    stopifnot(all.false || one.true)

    mode.bool
  },

  # create a result list, possibly with some members that are NULL
  create.result = function(status=NULL, mode=NULL, data.df=NULL, meta.list=NULL, std_meta=NULL) {
    result <- list()

    result[["status"]] <- status

    result[["mode"]] <- mode
    mode.vec <- self$determine.mode(mode)
    result[["is.absorbance"]] <- mode.vec[["is.absorbance"]]
    result[["is.reflectance"]] <- mode.vec[["is.reflectance"]]
    result[["is.transmittance"]] <- mode.vec[["is.transmittance"]]

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


