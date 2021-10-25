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
       status <- 4
       mode <- NULL

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

       super$create.result(status, mode, spec.df, meta.list)
     }
   )
)
