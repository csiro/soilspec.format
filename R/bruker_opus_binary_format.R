# Bruker Opus binary format, from Bruker MIR instrument
#
# See:
# - https://github.com/pierreroudier/opusreader
# - https://github.com/spectral-cockpit/opusreader2
# - https://www.bruker.com/en/products-and-solutions/infrared-and-raman/ft-ir-routine-spectrometer/what-is-ft-ir-spectroscopy.html

makeStandardMetaData_BrukerOpusBinary <- function(meta.list, wavenumbers, opus2, filename) {

  md <- createStandardMetadataContainer()

  md[['Sample_ID']] <- meta.list$sample_id
  md[['Spectra_ID']] <- meta.list$unique_id
  md[['spectra_source_file_name']] <- filename
  md[['DateTime']] <- opus2[[filename]]$basic_metadata$timestamp_string #opus2[[filename]]$basic_metadata$local_datetime
  md[["basic_metadata"]] <- str_flatten_comma(names(opus2[[filename]]$basic_metadata))

  # # DB Fields
  md[['instrument_technology_type']] <- 'MIR'
  md[['instrument_manufacturer']] <- 'Bruker'
  md[['instrument_model']] <- meta.list$instr_name_range
  md[['instrument_serial_number']] <- meta.list$SerialName
  md[['spectra_wavesignature_units']] <- 'wn'

  md[['instrument_min_wavelength']] <- min(wavenumbers)
  md[['instrument_max_wavelength']] <- max(wavenumbers)
  md[['spectra_temperature']] <- meta.list$temp_scanner_sm
  md[['spectra_humidity']] <- meta.list$hum_abs_sm

  return(md)

}

BrukerOpusBinary <- R6::R6Class("BrukerOpusBinary",
  inherit = SpectrumFormat,

  public = list(
    initialize = function() {
      super$initialize(origin = "Bruker",
                       type_name = "MIR",
                       suffix = ".0")
    },

    read = function(path) {
      spec.df <- NULL
      meta.list <- NULL
      mode <- NULL
      stdmeta <- NULL

      status <- super$file_status(path)

      if (status == 0) {
        status <- 4

        out <- tryCatch({
        },
        error=function(cond) {
        },
        warning=function(cond) {
        },
        finally={
          # initial metadata via opusreader
          spec.data <- opusreader::opus_read(path)
          if (!is.null(names(spec.data)) && length(names(spec.data)) != 0) {
            meta.list <- as.list(spec.data$metadata)

            # data and standard metadata via opusreader2
            opus2 <- opusreader2::read_opus(path)
            fname <- names(opus2)[1]

            mode <- opus2[[fname]]$acquisition$parameters$PLF$parameter_value

            if (mode == "AB") {
              no_atm_comp <- opus2[[fname]][["ab_no_atm_comp"]]
            } else {
              no_atm_comp <- opus2[[fname]][["refl_no_atm_comp"]]
            }

            intensities <- no_atm_comp[["data"]]
            wavenumbers <- no_atm_comp[["wavenumbers"]]

            stdmeta <- makeStandardMetaData_BrukerOpusBinary(meta.list, wavenumbers, opus2, fname)

            spec.df <- data.frame(wavenumber=wavenumbers, intensity=unlist(as.list(intensities)))

            status <- 0
          } else {
            status <- 2
          }
        })
      }

      super$create.result(status, mode, spec.df, meta.list, stdmeta)
    }
  )
)
