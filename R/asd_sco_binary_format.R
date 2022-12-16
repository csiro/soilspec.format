# ASD SCO binary format, e.g. for vis-NIR contact probe

ASDScoBinary <- R6::R6Class("ASDScoBinary",
  inherit = SpectrumFormat,

  public = list(
   initialize = function() {
     super$initialize(origin = "ASD",
                      type_name = "visNIR",
                      suffix = ".sco")
   },

   read = function(path) {
     spec.data <- NULL
     spec.df <- NULL
     meta.list <- NULL
     status <- 4
     mode <- NULL

     if (!file.exists(path)) {
       status <- 1
     } else if (file.info(path)$size != 0) {
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
         status <- 0
       })
     }

     super$create.result(status, mode, spec.df, meta.list)
  }
  )
)
