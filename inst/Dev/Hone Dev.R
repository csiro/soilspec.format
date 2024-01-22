library(stringr)
library(soilspec.format)


path <- hone.lab.red.file.path()
path <- asd.binary.file.path()
path <-  asd.sco.binary.file.path ()
path <-  bruker.opus.binary.file.path()
path <- nicolet.spa.file.path()
path <- thermo.spc.file.path()
path <- perkin.elmer.sp.pepe.file.path()

path <- unknown.file.path()
spec <- read.soilspec(path)

hone <- read.soilspec(path)



adf <- read.csv('C:/Projects/Spectra/Hone - ACIAR Pacific Soils_20231121_083528__001.csv')
colnames(adf)


remotes::install_github("pierreroudier/opusreader")
remotes::install_github("spectral-cockpit/opusreader2")

bruker.opus.binary.file.path()

pths <- c(hone.lab.red.file.path(),
          asd.binary.file.path(),
          asd.sco.binary.file.path (),
          bruker.opus.binary.file.path(),
          nicolet.spa.file.path(),
          thermo.spc.file.path(),
          perkin.elmer.sp.pepe.file.path(),
          perkin.elmer.sp.peir.file.path()
          )

outDir <- 'C:/Temp/SpectraMeta'
for (i in 1:length(pths)) {

  path <- pths[i]
  spec <- read.soilspec(path)
  str(spec)
  #ext <- strsplit(basename(p), split="\\.")[[1]][2]
  #fname <- str_remove(basename(p), paste0('.', ext))
  fname <- basename(dirname(p))
  m <- spec$metadata
  metaDf <- data.frame(tag=m, value=as.list(m))
  write.csv(t(as.data.frame(m)), paste0(outDir, '/', fname, '.csv'), row.names = T)
}

library(opusreader2)
asd <- read.soilspec(asd.binary.file.path())
bruckerBinary <- read.soilspec(bruker.opus.binary.file.path())


path <- 'C:/Projects/Spectra/Hone_Test_1.csv'
path <- 'C:/Users/sea084/AppData/Local/R/win-library/4.3/soilspec.format/extdata/HoneLabRed/example.hlr'
path <- hone.lab.red.file.path()
file.exists(path)

