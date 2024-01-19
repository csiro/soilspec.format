# ASD SCO binary format, e.g. for vis-NIR contact probe



makeStandardMetaData_ASD_SCO <- function(meta.list, filepath){

  md <- createStandardMetadataContainer()
  md[['spectra_source_file_name']] <- meta.list$name

  return(md)
}






ASDScoBinary <- R6::R6Class("ASDScoBinary",
  inherit = SpectrumFormat,

  public = list(
   initialize = function() {
     super$initialize(origin = "ASD",
                      type_name = "visNIR",
                      suffix = ".sco")
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
         spec.data <- t(prospectr::readASD(path))

         spec.df <-
            data.frame(wavenumber=as.integer(rownames(spec.data)),
                       intensity=spec.data[1:length(spec.data)])

         meta.list <- list()
         meta.list[["name"]] <- colnames(spec.data)
         stdmeta <- makeStandardMetaData_ASD_SCO(meta.list, path)
         status <- 0
       })
     }

     super$create.result(status, mode, spec.df, meta.list, stdmeta )
  }
  )
)
