library(DBI)
library(odbc)
library(jsonlite)
library(spectacles)
library(tidyverse)
library(signal)
library(soilspec.format)



doQuery <- function(con, sql){
  res <- dbSendQuery(con, sql)
  rows <- dbFetch(res)
  dbClearResult(res)
  return(rows)
}

pre_proc_nir <- function(spc,n,p,m){
  spc_abs <- data.frame(log(1/spc)) #absorbance
  spectra(spc_abs) <- ~350:2500
  nir_p <- spc_abs %>%
    spectacles::cut(wl =450:2450) %>%
    apply_spectra(sgolayfilt, n=n, p=p, m=m) %>%
    apply_spectra(snv)

  return(nir_p@nir)
}



####    Spectra DB Connection  #####
tcon <- DBI::dbConnect(odbc::odbc(),
                       Driver   = "ODBC Driver 17 for SQL Server",
                       Server   = "sql-asris-stage.it.csiro.au",
                       Database = 'CCDB_SPECTRAv2',
                       UID = "SPECTRA_READ",
                       PWD = 'Cl@ncy0ffTh3Overfl0w'
)

dbListTables(tcon)


res <- dbSendQuery(tcon, 'select * from CODES')
dbFetch(res)

nToReturn=100
bufferFraction = 0.2
att <- '4A1'
specType <- 'visNIR'
correctionID = 1
reductionID = 1
nDim=4


####### Get a subsample fro spectra table hopefully contiguous

ids <- c('83427;82843;83177;83200;83254;83481;82812;83341;83674;83060;83628;83094;82772;83144;82984;83199;82990;83035;82992;83070')

rawDBSpec <- getSpectraFromDB(ids)


###.................................... ####

####### Start The NIR Processing workflow  #####
NIR <- rawDBSpec[rawDBSpec$spectra_mode=='VisNIR',]


outDir <- 'c:/temp/specDFSamples'
if(!dir.exists(outDir)){dir.create(outDir)}


#####  Transform the individual spectra into a combined dataframe  ####
res <- vector("list", length = nrow(NIR))

for (i in 1:nrow(NIR)) {
  print(paste0(i, ' of ', nrow(NIR)))
  rawSpecFile <- fromJSON(NIR$spectra_json_obj[[i]])
  vals <- t(rawSpecFile$values)
  colnames(vals) <- t(rawSpecFile$wavesignatures)

  df <- data.frame(SpecID=NIR$spectra_id[i], vals, check.names = F)
  res[[i]] <- df
}

specDF = as.data.frame(data.table::rbindlist(res, fill=T))

saveRDS(specDF, paste0(outDir,'/NIRSpectraAsDF2.rds'))


