# Nicolet .spa format, from Nicolet MIR

NicoletSpa <- R6::R6Class("NicoletSpa",
  inherit = SpectrumFormat,
  public = list(
    initialize = function() {
      super$initialize(origin = "Nicolet",
                       type_name = "MIR",
                       suffix = ".spa")
    },

    create.standard.meta.data.container = function(meta.list, filepath){
      md <- super$create.standard.metadata.container()

      md[['spectra_source_file_name']] <- basename(filepath)
      resLine <- meta.list$Resolution
      bits <- stringr::str_split(resLine, ' ')

      if (length(bits) != 0) {
        md[['instrument_resolution']] <- bits[[1]][1]
        md[['instrument_min_wavelength']] <- bits[[1]][3]
        md[['instrument_max_wavelength']] <- bits[[1]][5]
        md[['spectra_wavesignature_units']] <- 'wn'
      }

      md
    },

    read = function(path) {
      status <- super$file_status(path)

      spec.df <- NULL
      meta.list <- NULL
      stdmeta <- NULL
      mode <- NULL

      if (status == 0) {
        result <- parse_nicolet_spa(path)
        status <- as.integer(result$status)

        if (status != 0) {
          status <- 4
        }

        spec.df <- data.frame(wavenumber=result$wavelengths, intensity=result$intensities)

        meta.list <- key.value.pairs(path)
        mode <- meta.list[["Final format"]]
        stdmeta <- self$create.standard.meta.data.container(meta.list, path)

        if (stdmeta[['spectra_wavesignature_units']] != 'wn') {
          status <- 2
        }
      }

      super$create.result(status, mode, spec.df, meta.list, std_meta=stdmeta)
    }
  )
)

# Extract key-value pairs from a .spa file and return them as a dictionary
# given path. A key-value pair consists either of "X = Y" or "X: Y" here.
# There may be a more formal binary file position way to improve this.
key.value.pairs <- function(path) {
  key2value <- list()

  for (str in strings.from.spa(path)) {
    delimiter <- NULL
    if (grepl(str, pattern="=")) {
      delimiter <- "="
    } else if (grepl(str, pattern=":")) {
      delimiter <- ":"
    }

    if (!is.null(delimiter)) {
      fields <- stringr::str_split(str, pattern=delimiter)
      keyval <- fields[[1]]
      key <- stringr::str_squish(keyval[1])
      # if there is more than one value, the delimiter must be present in str more than once,
      # e.g. C:\\a\b\c
      value <- stringr::str_squish(paste(keyval[2:length(keyval)], collapse=delimiter))
      key2value[[key]] <- value

    }
  }
return(key2value)

}

# Return list of strings of minimum length with specified characters excluded
strings.from.spa <- function(path, min.str.len=6, chrs.to.exclude=c("?", ";", "$")) {
  strings <- list()
  index <- 1
  str <- ""
  last <- as.raw(0)

  for (byte in bytes.from.spa(path)) {
    # convert from raw value to integer
    #byte <- as.integer(raw.byte)

    if (byte == 9) {
      # tab => space
      byte <- as.raw(32)
    } else if (byte == 13) {
      # start of CR LF sequence
      last <- byte
    } else if (last == 13 && byte == 10 && !grepl(str, pattern="^\\s*$")) {
      # CR LF sequence ends string; ignore whitespace strings
      strings[[index]] <- str
      index <- index + 1
      str <- ""
      last <- 0
    } else if (byte >= 32 && byte <= 127) {
      # collect a character
      str <- paste0(str, rawToChar(byte))
      #str <- remove.formatting.markers(str)
      last <- byte
    } else {
      # anything else ends string
      if (length(str) >= min.str.len) {
        if (!any(unlist(lapply(chrs.to.exclude,
                               function(partial.str) {
                                 grepl(str, fixed=T, pattern=partial.str)
                               })))) {
          # no exclusion chars exist in string, so retain it
          strings[[index]] <- str

          index <- index + 1
        }
      }
      str <- ""
      last <- 0
    }
  }
  strings

}

remove.formatting.markers <- function(str) {
  str <- stringr::str_replace(str, pattern = "\\\\par", replacement = "")
  stringr::str_replace(str, pattern = "\\\\tab", replacement = "")
}

# Return bytes from file
bytes.from.spa <- function(path) {
  file.con <- file(path, "rb")
  raw.bytes <- readBin(con = file.con, n = file.info(path)$size, what = "raw")
  close(file.con)
  raw.bytes
}
