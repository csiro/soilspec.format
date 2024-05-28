# ASD SCO binary format, e.g. for vis-NIR contact probe



makeStandardMetaData_SCANS <- function(meta.list, spec.data){

  md <- createStandardMetadataContainer()

  bits <- stringr::str_split(spec.data$core.label, '_')

  md[['Sample_ID']] <- paste0(bits[[1]][1], '_', bits[[1]][2])
  md[['Spectra_ID']] <- stringr::str_remove(meta.list$name, '.scan')
  md[['spectra_source_file_name']] <- meta.list$name
  md[['spectra_wavesignature_units']] <- 'nm'

  return(md)
}






SCANS <- R6::R6Class("SCANS",
  inherit = SpectrumFormat,

  public = list(
   initialize = function() {
     super$initialize(origin = "SCANS",
                      type_name = "visNIR",
                      suffix = ".scan")
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
         spec.data <- read.csv(path)

        cidxs <- which(stringr::str_starts(colnames(spec.data), 'R'))
        wvs <- as.integer(stringr::str_remove(colnames(spec.data)[cidxs], 'R'))
        vls <- as.numeric(spec.data[1,cidxs])
         spec.df <-
            data.frame(wavenumber=wvs,
                       intensity=vls
                       )


         meta.list <- list()
         meta.list[["name"]] <- basename(path)
         stdmeta <- makeStandardMetaData_SCANS(meta.list, spec.data)

         #Insert splice correction
         spec.df$intensity <- prospectr::spliceCorrection(X=spec.df$intensity,
                                                          wav=spec.df$wavenumber,
                                                          splice = c(1000,1830))



         status <- 0
       })
     }

     super$create.result(status, mode, spec.df, meta.list, stdmeta )
  }
  )
)
