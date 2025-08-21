makeStandardMetaData_SED <- function(meta.list, filepath){

  md <- createStandardMetadataContainer()
  md[['spectra_source_file_name']] <- basename(filepath)

  wavelength.range <- meta.list[["Wavelength Range"]]
  wavelength.minmax <- stringr::str_split(wavelength.range, ",", n = 2)[[1]]
  md[['instrument_min_wavelength']] <- wavelength.minmax[1]
  md[['instrument_max_wavelength']] <- wavelength.minmax[2]
  md[['spectra_wavesignature_units']] <- meta.list[["Units"]]

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

        # read data, skipping over metadata + "Data:" + column header lines
        spec.df <- parse.sed(path, length(meta.list)+2)

        mode <- meta.list[["Measurement"]]
        stdmeta <- makeStandardMetaData_SED(meta.list, path)
      }

      super$create.result(status, mode, spec.df, meta.list, std_meta=stdmeta)
    }
  )
)

parse.sed <- function(path, skip.n.lines) {
  df <- read.csv(path, skip = skip.n.lines, sep = "",
                 col.names = c("wavenumber", "intensity"), )

  intensities <- df$intensities
  intensity.colname <- colnames(df)[2]

  if (stringr::str_sub(intensity.colname,
                       nchar(intensity.colname), 1) == "%") {
    intensities <- intensities / 100;
    df[intensity.colname] <- intensities
  }

  return(df)
}

# Extract metadata key-value pairs from a .sed file and return them as a dictionary
# given path. A key-value pair consists of "X: Y" here.
extract.metadata <- function(path) {
  key2value <- list()

  for (line in readLines(path)) {
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
      break
    }
  }

  key2value
}
