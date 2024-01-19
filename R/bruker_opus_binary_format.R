# Bruker Opus binary format, from Bruker MIR instrument
#
# See:
# - https://github.com/pierreroudier/opusreader
# - https://github.com/spectral-cockpit/opusreader2
# - https://www.bruker.com/en/products-and-solutions/infrared-and-raman/ft-ir-routine-spectrometer/what-is-ft-ir-spectroscopy.html



makeStandardMetaData_BrukerOpusBinary <- function(meta.list, opus2, filepath){

  md <- createStandardMetadataContainer()

  md[['Sample_ID']] <- meta.list$sample_id
  md[['Spectra_ID']] <- meta.list$unique_id
  md[['spectra_source_file_name']] <- basename(filepath)
  md[['DateTime']] <- opus2$example.0$basic_metadata$local_datetime


  # # DB Fields

  md[['instrument_technology_type']] <- 'MIR'
  md[['instrument_manufacturer']] <- 'Bruker'
  md[['instrument_model']] <- meta.list$instr_name_range
  md[['instrument_serial_number']] <- meta.list$SerialName

  waveNums <- opus2$example$ab_no_atm_comp$wavenumbers
  md[['instrument_min_wavelength']] <- min(waveNums)
  md[['instrument_max_wavelength']] <- max(waveNums)

  md[['spectra_temperature']] <- meta.list$temp_scanner_sm
  md[['spectra_humidity']] <- meta.list$hum_abs_sm

  return(md)

}

# library(listviewer)
# jsonedit( opus2 )




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
          # metadata via opusreader
          spec.data <- opusreader::opus_read(path)
          meta.list <- as.list(spec.data$metadata)


          # data via opusreader2
          opus2 <- opusreader2::read_opus(path)
          stdmeta <- makeStandardMetaData_BrukerOpusBinary(meta.list, opus2, path)
          fname <- names(opus2)[1]
          ab_no_atm_comp <- opus2[[fname]][["ab_no_atm_comp"]]
          intensities <- ab_no_atm_comp[["data"]]
          wavenumbers <- ab_no_atm_comp[["wavenumbers"]]
          spec.df <- data.frame(wavenumber=wavenumbers, intensity=unlist(as.list(intensities)))

          # mode via opusreader2
          mode <- opus2[[fname]]$acquisition$parameters$PLF$parameter_value

          status <- 0
        })
      }

      super$create.result(status, mode, spec.df, meta.list, stdmeta)
    }
  )
)
