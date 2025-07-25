# Generic CSV. Assumes 2 unnamed columns of real data separated by commas.

CSV <- R6::R6Class("CSV",
   inherit = SpectrumFormat,

   public = list(
     initialize = function() {
       super$initialize(origin = "Unknown",
                        type_name = "Generic",
                        suffix = ".csv")
     },

     mode.bool.to.str = function(is.absorbance, is.reflectance, is.transmittance) {
         mode <- NULL

         if (is.absorbance) {
           mode <- "ab"
         } else if (is.reflectance) {
           mode <- "rfl"
         } else if (is.transmittance) {
           mode <- "tran"
         }

         mode
     },

     read.df = function(path) {
       # Is there a header?
       # If the first line doesn't start with a number, assume it's a header.
       line <- readLines(path, n = 1)
       match <- unlist(gregexpr("[0-9]", substr(line, 1, 1)))
       has.header <- match == -1

       # Read data frame with or without header
       df <- read.csv(path, header = has.header, sep = ",")

       df
     },

     read = function(path,
                     is.absorbance = F, is.reflectance = F, is.transmittance = F,
                     source.col.names = c("wavenumber", "intensity")) {
       spec.df <- NULL
       meta.list <- NULL
       mode <- self$mode.bool.to.str(is.absorbance, is.reflectance, is.transmittance)
       stdmeta <- createStandardMetadataContainer()  ### raw spec file does not contain any metadata so just
                                                     #   returning and empty standard metadata object for consistency
       status <- super$file_status(path)

       if (status == 0) {
         status <- 4

         out <- tryCatch({
           spec.df <- self$read.df(path)

           if (ncol(spec.df) > 2) {
             # Extract only the columns we want from.
             # This will only work if the file has a header.
             spec.df <- spec.df[source.col.names]
           }
           if (ncol(spec.df) != 2) {
             # Not two columns even now?
             status <- 2
           } else {
             # All fine, so name the columns.
             colnames(spec.df) <- c("wavenumber", "intensity")
             meta.list <- list()
             status <- 0
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
