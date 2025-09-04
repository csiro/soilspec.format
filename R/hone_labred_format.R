##################################################################
#####       Author : Ross Searle                             #####
#####       Date :  Wed Dec 13 14:46:34 2023                 #####
#####       Purpose : Reads a raw spectra file from the Hone #####
#####                 Lab Red NIR instrument                 #####
#####       Comments :  wavelength range is 1350 - 2550      #####
##################################################################

HoneLabRed <- R6::R6Class("HoneLabRed",
   inherit = SpectrumFormat,

   public = list(
     initialize = function() {
       super$initialize(origin = "Hone Lab Red",
                        type_name = "NIR",
                        suffix = ".hlr")
     },

     create.standard.meta.data.container = function(meta.list, filepath){
       md <- super$create.standard.metadata.container()

       md[['sample_id']] <- meta.list$Sample.ID
       md[['spectra_id']] <- meta.list$SpectrumId
       md[['spectra_source_file_name']] <- basename(filepath)

       rawDate <- meta.list$Date.Time
       bits <- stringr::str_split(rawDate, '_')
       dt <- sapply(bits, function (x) x[1])
       tm <- sapply(bits, function (x) x[2])
       d1 <- as.POSIXct(paste0(stringr::str_sub(dt, 1,4), '-', stringr::str_sub(dt, 5,6), '-', stringr::str_sub(dt, 7,8), ' ',
                               stringr::str_sub(tm, 1,2), ':', stringr::str_sub(tm, 3,4), ':', stringr::str_sub(tm, 5,6)),
                        tz = Sys.timezone())
       md[['date_time']] <- format(d1, format = '%d-%m-%Y %H:%M:%S')

       # # DB Fields

       md[['instrument_technology_type']] <- 'NIR'
       md[['instrument_manufacturer']] <- 'HoneAg'
       md[['instrument_model']] <- meta.list$HlrType
       md[['instrument_serial_number']] <- meta.list$SerialName
       md[['instrument_resolution']] <- '16-20 nm'
       md[['instrument_min_wavelength']] <- '1350'
       md[['instrument_max_wavelength']] <- '2550'
       md[['instrument_units']] <- 'nm'
       md[['spectra_wavesignature_units']] <- 'nm'

       md[['spectra_temperature']] <- meta.list$NeoTemperaturePost
       md[['spectra_humidity']] <- ''

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
           stdmeta <- self$create.standard.meta.data.container(meta.list, path)

           status <- 0

         },
         error=function(cond) {
         },
         warning=function(cond) {
         },
         finally={
         })
       }



       super$create.result(status=status, mode=mode, data.df=spec.df, meta.list=meta.list, std_meta=stdmeta)
     }
   )
)
