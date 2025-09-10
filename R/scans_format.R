# CSIRO SCANS format

SCANS <- R6::R6Class("SCANS",
  inherit = SpectrumFormat,

  public = list(
   initialize = function() {
     super$initialize(origin = "SCANS",
                      type_name = "visNIR",
                      suffix = ".scan")
   },

   create.standard.meta.data.container = function(meta.list, spec.data){
     md <- super$create.standard.metadata.container()

     bits <- stringr::str_split(spec.data$core.label, '_')

     md[['sample_id']] <- paste0(bits[[1]][1], '_', bits[[1]][2])
     md[['spectra_id']] <- stringr::str_remove(meta.list$name, '.scan')
     md[['spectra_source_file_name']] <- meta.list$name
     md[['spectra_wavesignature_units']] <- 'nm'

     md
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
         stdmeta <- self$create.standard.meta.data.container(meta.list, spec.data)

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
