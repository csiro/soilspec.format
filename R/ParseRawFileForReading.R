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


#parseHoneLabRed <- function(path = path, outDirectory = outDirectory)
#' @title Parse a raw Hone Lab Red spectra file containing multiple spectra
#' @description  Parses a raw Hone Lab Red spectra file containing multiple spectra into individual files containing a single spectra for reading in read.soilspec() function
#' @export
#' @param path Full path to the raw Hone Lab Red spectra file
#' @param outDirectory Full path to the output directory
#' @return A list of file names each containing 1 sample and 1 background record

parseHoneLabRed <- function(path, outDirectory) {

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
    bkgDF <- specdf[idxB,]

    idxS <-  which(specdf$Type=='SAMPLE')
    sampDF <- specdf[idxS,]

    for (j in 1:nrow(sampDF)) {
      srec = sampDF[j,]
      brec = specdf[1,]

      fname = paste0(srec$'Sample ID', '_', srec$SampleSpectrumSetId, '_', srec$SpectrumId)
      odf <- rbind(brec, srec)
      write.csv(odf, paste0(outDirectory, '/', fname, '.hlr'), row.names = F)

      }
  }
}





