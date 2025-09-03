create.standard.meta.data_SED <- function(meta.list, filepath){

  md <- create.standard.metadata.container()
  md[['spectra_source_file_name']] <- basename(filepath)

  # We take the rightmost (second) date and time because it appears to be
  # the time for the sample spectrum vs background.
  md['date.time'] <- paste(stringr::str_split(meta.list$Date, ",")[[1]][2],
                          stringr::str_split(meta.list$Time, ",")[[1]][2])

  wavelength.range <- meta.list[['Wavelength Range']]
  wavelength.minmax <- stringr::str_split(wavelength.range, ",", n = 2)[[1]]
  md[['instrument_min_wavelength']] <- as.double(wavelength.minmax[1])
  md[['instrument_max_wavelength']] <- as.double(wavelength.minmax[2])
  md[['instrument_model']] <- meta.list[['Instrument']]
  md[['instrument_manufacturer']] <- 'Spectral Evolution'
  md[['instrument_units']] <- 'nm'
  md[['spectra_wavesignature_units']] <- 'nm'

  md
}

SpectralEvolution <- R6::R6Class("SpectralEvolution",
  inherit = SpectrumFormat,
  public = list(
    initialize = function() {
      super$initialize(origin = "Spectral Evolution",
                       type_name = "visNIR",
                       suffix = ".sed")
    },

    read = function(path) {
      status <- super$file_status(path)

      spec.df <- NULL
      meta.list <- NULL
      stdmeta <- NULL
      mode <- NULL

      if (status == 0) {
        meta.list <- extract.metadata(path)

        # Read data, skipping over metadata, which does not include the "Data:"
        # line, but does include an entry based upon the data header (see
        # "intensity_is_percentage"), therefore is the length of the metadata
        # in terms of number of lines.
        spec.df <- parse.sed(path, length(meta.list),
                             meta.list[["intensity_is_percentage"]])

        mode <- meta.list[["Measurement"]]
        stdmeta <- create.standard.meta.data_SED(meta.list, path)
      }

      super$create.result(status, mode, spec.df, meta.list, std_meta=stdmeta)
    }
  )
)

# Given a .sed file path, number of lines to skip
parse.sed <- function(path, skip.n.lines, intensity_is_percentage) {
  df <- read.csv(path, skip = skip.n.lines, sep = "")

  if (intensity_is_percentage) {
    divisor <- 100
  } else {
    divisor <- 1
  }

  spec.df <- data.frame(wavenumber=df[,1], intensity=df[,2] / divisor)

  spec.df
}

# Extract metadata key-value pairs from a .sed file and return them as
# a dictionary given path. A key-value pair consists of "X: Y" here.
extract.metadata <- function(path) {
  key2value <- list()

  # We don't want to read all lines, just metadata lines, but without
  # a formal file format specification, we don't know how many lines
  # there are. We will only process as many as needed however. Yes,
  # loops in R are not ideal, but this is a small list (~2000 elements).
  lines <- readLines(path)

  i <- 1
  while (i <= length(lines)) {
    line <- lines[i]
    if (!startsWith(line, "Data:")) {
      field.list <- stringr::str_split(line, ": ", n = 2)

      if (length(field.list) == 1) {
        fields <- field.list[[1]]
        key <- fields[1]
        value <- fields[2]
        if (!is.na(value)) {
          if (value == "n/a" || value == "none" || value == "") value <- NA
        }

        key2value[[key]] <- value
      }
    } else {
      # Does header line end with "%"
      # e.g. Wvl Reflect. %
      key2value[["intensity_is_percentage"]] <-
        length(grep("%$", lines[i+1])) == 1

      break
    }
    i <- i + 1
  }

  key2value
}
