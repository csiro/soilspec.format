# ASD binary format, e.g. for vis-NIR contact probe


makeStandardMetaData_ASD <- function(meta.list, filepath){

  md <- createStandardMetadataContainer()

  md[['Sample_ID']] <- ''
  md[['Spectra_ID']] <- ''
  md[['spectra_source_file_name']] <- basename(filepath)
  md[['Response']] <- meta.list$data_type
  md[['DateTime']] <- meta.list$dc_time

   # DB Fields

  md[['instrument_technology_type']] <- 'visNIR'
  md[['instrument_manufacturer']] <- 'ASD'
  md[['instrument_model']] <- meta.list$instrument
  md[['instrument_serial_number']] <- meta.list$instrument_num
  md[['instrument_min_wavelength']] <- meta.list$ch1_wavel
  md[['instrument_max_wavelength']] <- as.numeric(meta.list$ch1_wavel) + as.numeric(meta.list$channels) - 1

  return(md)

}







ASDBinary <- R6::R6Class("ASDBinary",
  inherit = SpectrumFormat,

  public = list(
    initialize = function() {
      super$initialize(origin = "ASD",
                       type_name = "visNIR",
                       suffix = ".asd")
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
          spec.data <- asdreader::get_spectra(path)
          spec.data <- t(spec.data)

          table <- cbind(x=rownames(spec.data), y=spec.data[,])
          spec.df <- data.frame(wavenumber=as.double(table[,1]),
                                intensity=as.double(table[,2]))
          rownames(spec.df) <- NULL

          meta.list <- asdreader::get_metadata(path)
          stdmeta <- makeStandardMetaData_ASD(meta.list, path)

          mode <- as.character(meta.list$data_type)
          status <- 0
        },
        error=function(cond) {
        },
        warning=function(cond) {
        },
        finally={
        })
      }

      super$create.result(status, mode, spec.df, meta.list, std_meta=stdmeta)
    }
  )
)
