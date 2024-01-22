# Generic CSV. Assumes 2 unnamed columns of real data separated by commas.

CSV <- R6::R6Class("CSV",
   inherit = SpectrumFormat,

   public = list(
     initialize = function() {
       super$initialize(origin = "Unknown",
                        type_name = "Generic",
                        suffix = ".csv")
     },

     read = function(path) {
       spec.df <- NULL
       meta.list <- NULL
       mode <- NULL
       stdmeta <- createStandardMetadataContainer()  ### raw spec file does not contain any metadata so just
                                                     #   returning and empty standard metadata object for consistency
       status <- super$file_status(path)

       if (status == 0) {
         status <- 4

         out <- tryCatch({
          spec.df <- read.csv(path, header = F, sep = ",", col.names = c("wavenumber", "intensity"))
          if (nrow(spec.df) != 0) {
            if (ncol(spec.df) != 2) {
              status <- 2
            } else {
              meta.list <- list()
              status <- 0
            }
          }
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
