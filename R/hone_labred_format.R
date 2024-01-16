##################################################################
#####       Author : Ross Searle                             #####
#####       Date :  Wed Dec 13 14:46:34 2023                 #####
#####       Purpose : Reads a raw spectra file from the Hone #####
#####                 Lab Red NIR instrument                 #####
#####       Comments :  wavelength range is 1350 - 2550      #####
##################################################################

library(stringr)


HoneLabRed <- R6::R6Class("HoneLabRed",
                   inherit = SpectrumFormat,

                   public = list(
                     initialize = function() {
                       super$initialize(origin = "Hone Lab Red",
                                        type_name = "NIR",
                                        suffix = ".hlr")
                     },

                     read = function(path) {

                       spec.df <- NULL
                       meta.list <- NULL
                       mode <- NULL

                       status <- super$file_status(path)

                       if (status == 0) {
                         status <- 4

                         out <- tryCatch({
                           df <- read.csv(path, header = T, sep = ",")

                           stopifnot(nrow(df)==2)

                           bgRec <- df[df$Type=='BACKGROUND', ]
                           smpRec <- df[df$Type=='SAMPLE', ]

                           if(nrow(bgRec) != 1 | nrow(smpRec) != 1){
                             stop('The Hone Lab Red spectra file needs to contain a background and a sample record')
                           }

                           sdf <- as.data.frame( t(smpRec) )
                           bdf <- as.data.frame( t(bgRec) )

                           smpVals <- sdf[40:nrow(sdf),1]
                           bgVals <- bdf[40:nrow(bdf),1]
                           spectable <- as.double(smpVals) / as.double(bgVals)
                           nms <- stringr::str_remove(rownames(sdf)[40:nrow(sdf)], 'X')
                           spec.df <- data.frame(wavenumber=as.double(nms), intensity=spectable)

                           b <- as.data.frame(sdf)
                           ml <- as.list(b[[1]])
                           names(ml) <- rownames(b)
                           meta.list <- ml[1:39]
                           status <- 0

                         },
                         error=function(cond) {
                         },
                         warning=function(cond) {
                         },
                         finally={
                         })
                       }

                       super$create.result(status=status, mode=mode, spec.df, meta.list)
                     }
                   )
)
