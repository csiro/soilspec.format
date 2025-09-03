##################################################################
#####       Author : J Moloney                               #####
#####       Date :  Wed July 23 2025                         #####
#####       Purpose : Reads a reduced wavelength raw spectra #####
#####                 file from the Hone                     #####
#####                 Lab Red NIR instrument                 #####
#####       Comments :  wavelength range is 1350 - 2550      #####
##################################################################



create.standard.meta.data_HoneLabRedReduced <- function(meta.list, filepath){

  md <- create.standard.metadata.container()

  md[['sample.id']] <- meta.list$Sample.ID
  md[['spectra.id']] <- meta.list$NIRId
  md[['spectra_source_file_name']] <- basename(filepath)

  rawDate <- meta.list$Date.Time

  md[['date.time']] <- format(rawDate, format = '%d-%m-%Y %H:%M:%S')


  md[['instrument_technology_type']] <- 'NIR'
  md[['instrument_manufacturer']] <- 'HoneAg'
  md[['instrument_model']] <- stringr::str_split_i(i = 1,pattern="-",string = meta.list$DeviceName)
  md[['instrument_serial_number']] <- meta.list$DeviceName
  md[['instrument_resolution']] <- '16-20 nm'
  md[['instrument_min_wavelength']] <- '1350'
  md[['instrument_max_wavelength']] <- '2550'
  md[['instrument_units']] <- 'nm'
  md[['spectra_wavesignature_units']] <- 'nm'
  md[['spectra_temperature']] <- ''
  md[['spectra_humidity']] <- ''
  if(meta.list$is.Reflectance==T){
  md[['spectra_mode']] <- 'reflectance'
  }

  return(md)

}


HoneLabRedReduced <- R6::R6Class("HoneLabRedReduced",
                          inherit = SpectrumFormat,

                          public = list(
                            initialize = function() {
                              super$initialize(origin = "Hone Lab Red Reduced",
                                               type_name = "NIR",
                                               suffix = ".hlrr")
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

                                  stopifnot(nrow(df)==1)

                                  smpRec <- df[df$is.Reflectance==T, ]

                                  if(nrow(smpRec) != 1){
                                    stop('The Hone Lab Red Reduced spectra file should contain exactly 1 row of observations')
                                  }

                                  sdf <- as.data.frame( t(smpRec) )


                                  smpVals <- sdf[10:nrow(sdf),1]
                                  spectable <- as.double(smpVals)
                                  nms <- stringr::str_remove(rownames(sdf)[10:nrow(sdf)], 'X')
                                  spec.df <- data.frame(wavenumber=as.double(nms), intensity=spectable)

                                  b <- as.data.frame(sdf)
                                  ml <- as.list(b[[1]])
                                  names(ml) <- rownames(b)
                                  meta.list <- ml[1:9]
                                  stdmeta <- create.standard.meta.data_HoneLabRedReduced(meta.list, path)

                                  status <- 0

                                },
                                error=function(cond) {
                                },
                                warning=function(cond) {
                                },
                                finally={
                                })
                              }

                              if(length(stdmeta[['spectra_mode']])>0){
                                mode <- stdmeta[['spectra_mode']]
                              }


                              super$create.result(status=status, mode=mode, data.df=spec.df, meta.list=meta.list, std_meta=stdmeta)
                            }
                          )
)
