########################################################################
#####       Author : Ross Searle                                   #####
#####       Date :  Wed Jan 17 14:46:34 2024                       #####
#####       Purpose : Parses a raw spectra file from the Hone      #####
#####                 Lab Red NIR instrument to seperate out       #####
#####                 into individual files containing individual  #####
#####                 SAMPLE and BACKGROUND records for formating  #####
#####                 in the read.soilspec() function              #####
#####                                                              #####
########################################################################


#parse.hone.lab.red <- function(path = path, outDirectory = outDirectory)
#' @title Parse a raw Hone Lab Red spectra file containing multiple spectra
#' @description  Parses a raw Hone Lab Red spectra file containing multiple spectra into individual files containing a single spectra for reading in read.soilspec() function
#' @export
#' @param path Full path to the raw Hone Lab Red spectra file
#' @param outDirectory Full path to the output directory
#' @return A list of file names each containing 1 sample and 1 background record

parse.hone.lab.red <- function(path, outDirectory) {

if(!dir.exists(outDirectory)){dir.create(outDirectory)}

  indf <- read.csv(path, header = T, check.names=F)

  SpecSets <- unique(indf$SampleSpectrumSetId)

  for(i in 1:length(SpecSets)){
    print(paste0('Processing Spectral Set ', i, ' of ', length(SpecSets)))
    specid <- SpecSets[i]
    idxs <- which(indf$SampleSpectrumSetId==specid)
    specdf <- indf[idxs,]

    #####  Need to ask the experts what to do when there is more than one background record
    idxB <- which(specdf$Type=='BACKGROUND')
    bkgDF <- specdf[max(idxB),]

    idxS <-  which(specdf$Type=='SAMPLE')
    sampDF <- specdf[idxS,]

    for (j in 1:nrow(sampDF)) {
      srec = sampDF[j,]
      #brec = specdf[1,]

      fname = paste0(srec$'Sample ID', '_', srec$SampleSpectrumSetId, '_', srec$SpectrumId)
      odf <- rbind(bkgDF, srec)
      write.csv(odf, paste0(outDirectory, '/', fname, '.hlr'), row.names = F)

    }


  }

  idxs <- which(indf$Type=='BACKGROUND')
  indf <- indf[-idxs,]

  bits <- stringr::str_split(indf$`Date Time`, '_')
  rdts <- sapply(bits, function(x) paste0(x[1]))
  d <- stringr::str_sub(rdts, 7,8)
  m <- stringr::str_sub(rdts, 5,6)
  y <- stringr::str_sub(rdts, 1,4)

  sites <- sapply(bits, function(x) paste0(x[1], "_", x[2]))
  odfm <- data.frame(Sname=indf$`Sample ID`,	Upper=NA,	Lower=NA,	Lat=NA,	Lon=NA,	Datum='GDA94',
                     Filename=paste0(indf$'Sample ID', '_', indf$SampleSpectrumSetId, '_', indf$SpectrumId, '.hlr'),	Date=paste0(d, '/', m, '/',y))
  print(paste0(paste0(outDirectory, '/submit.meta')))
  write.csv(odfm,  paste0(outDirectory, '/submit.meta'), row.names = F)
}




########################################################################
#####       Author : Ross Searle                                   #####
#####       Date :  Wed Jan 17 14:46:34 2024                       #####
#####       Purpose : Parses a raw spectra file from the Hone      #####
#####                 Lab Red NIR instrument to seperate out       #####
#####                 into individual files containing individual  #####
#####                 SAMPLE and BACKGROUND records for formating  #####
#####                 in the read.soilspec() function              #####
#####                                                              #####
########################################################################


#parse.scans <- function(path = path, outDirectory = outDirectory)
#' @title Parse a raw SCANS spectra file containing multiple spectra
#' @description  Parses a raw SCANS spectra file containing multiple spectra into individual files containing a single spectra for reading in read.soilspec() function
#' @export
#' @param path Full path to the raw SCANS spectra file
#' @param outDirectory Full path to the output directory
#' @return A list of file names each containing 1 sample and 1 background record

parse.scans <- function(path, outDirectory) {

  if(!dir.exists(outDirectory)){dir.create(outDirectory)}

  indf <- read.csv(path, header = T, check.names=F)

  for(i in 1:nrow(indf)){
    rec <- indf[i,]
    print(paste0('Processing Spectra ', i, ' of ', nrow(indf)))
    specid <- paste0( rec$core.label, '_',  rec$core_position)
    fname <- paste0(specid, '.scan')
    odf<-rec
    write.csv(odf, paste0(outDirectory, '/', fname), row.names = F)
  }

  bits <- stringr::str_split(indf$core.label, '_')
  sites <- sapply(bits, function(x) paste0(x[1], "_", x[2]))

  dts <- format(Sys.Date(), "%d/%m/%Y")
  odfm <- data.frame(Sname=sites,	Upper=indf$core_position,	Lower=indf$core_position+1,	Lat=NA,	Lon=NA,	Datum='GDA94',
                     Filename= paste0( indf$core.label, '_',  indf$core_position, '.scan'),	Date=NA)
  write.csv(odfm,  paste0(outDirectory, '/submit.meta'), row.names = F)
}




