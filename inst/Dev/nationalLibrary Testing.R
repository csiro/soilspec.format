library(DBI)
library(odbc)
library(jsonlite)
library(spectacles)
library(tidyverse)
library(signal)
library(soilspec.format)
library(jsonview)



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

#
# tcon <- DBI::dbConnect(odbc::odbc(),
#                        Driver   = "ODBC Driver 17 for SQL Server",
#                        Server   = "sql-asris-stage.it.csiro.au",
#                        Database = 'CCDB_SPECTRAv2',
#                        Trusted_Connection = "yes")




####  Queries to select spectra with matching chem data ########

sql <- "SELECT o.agency_code, o.proj_code, o.s_id, o.o_id, lr.h_no, lr.samp_no, lr.labr_no, lr.labm_code, o.o_latitude_GDA94, o.o_longitude_GDA94, s.samp_lower_depth, s.samp_upper_depth, o.o_date_desc, lm.LABM_NAME, lr.labr_value, sp.spectra_id, sp.spectra_type, sp.method_id,
             sp.spectra_source_file_name, sp.spectra_mode, sp.spectra_wavesignature_units, sp.spectra_json_obj
FROM   dbo.OBSERVATIONS AS o INNER JOIN
             dbo.SAMPLES AS s ON s.agency_code = o.agency_code AND s.proj_code = o.proj_code AND s.s_id = o.s_id AND s.o_id = o.o_id INNER JOIN
             dbo.LAB_RESULTS AS lr ON lr.agency_code = s.agency_code AND lr.proj_code = s.proj_code AND lr.s_id = s.s_id AND lr.o_id = s.o_id AND lr.h_no = s.h_no AND lr.samp_no = s.samp_no INNER JOIN
             dbo.LAB_METHODS AS lm ON lm.LABM_CODE = lr.labm_code INNER JOIN
             dbo.SPECTRA AS sp ON sp.agency_code = s.agency_code AND sp.proj_code = s.proj_code AND sp.s_id = s.s_id AND sp.o_id = s.o_id AND sp.h_no = s.h_no AND sp.samp_no = s.samp_no
WHERE (lr.labm_code = N'4A1')"


sql <- "SELECT o.agency_code, o.proj_code, o.s_id, o.o_id, lr.h_no, lr.samp_no, lr.labr_no, lr.labm_code, o.o_latitude_GDA94, o.o_longitude_GDA94, s.samp_lower_depth, s.samp_upper_depth, o.o_date_desc, lm.LABM_NAME, lr.labr_value, sp.spectra_id, sp.spectra_type, sp.method_id,
             sp.spectra_source_file_name
FROM   dbo.OBSERVATIONS AS o INNER JOIN
             dbo.SAMPLES AS s ON s.agency_code = o.agency_code AND s.proj_code = o.proj_code AND s.s_id = o.s_id AND s.o_id = o.o_id INNER JOIN
             dbo.LAB_RESULTS AS lr ON lr.agency_code = s.agency_code AND lr.proj_code = s.proj_code AND lr.s_id = s.s_id AND lr.o_id = s.o_id AND lr.h_no = s.h_no AND lr.samp_no = s.samp_no INNER JOIN
             dbo.LAB_METHODS AS lm ON lm.LABM_CODE = lr.labm_code INNER JOIN
             dbo.SPECTRA AS sp ON sp.agency_code = s.agency_code AND sp.proj_code = s.proj_code AND sp.s_id = s.s_id AND sp.o_id = s.o_id AND sp.h_no = s.h_no AND sp.samp_no = s.samp_no"

#.####
####   Get all of the raw spectra out of the DB  ####
sql <- 'select * from Spectra'
rawDBSpec <- doQuery(tcon, sql)
saveRDS(rawDBSpec, 'C:/Projects/Spectra/workflowTesting/DBDumpofSpectraTable.rds')

rawDBSpec <- readRDS('C:/Projects/Spectra/workflowTesting/DBDumpofSpectraTable.rds')

MIR <- rawDBSpec[rawDBSpec$spectra_type=='MIR',]

specIDS <- sample(MIR$spectra_id, 20)

###.................................... ####

####### Start The NIR Processing workflow  #####
NIR <- rawDBSpec[rawDBSpec$spectra_type=='VisNIR',]


outDir <- 'c:/temp/specDF'
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

saveRDS(specDF, 'C:/Projects/Spectra/workflowTesting/NIRSpectraAsDF.rds')


####### Tidy up the spectra in readiness for PCA processing  ####
specDF <- readRDS('C:/Projects/Spectra/workflowTesting/NIRSpectraAsDF.rds')

# remove cases with missing spectra data
idxs <- which(!complete.cases(specDF[,which(names(specDF)=="350"):which(names(specDF)=="2500")]))
idxs
specDF <- specDF[-idxs,]
nrow(specDF)

# remove infinite cases
infs <- which(is.infinite(specDF[,which(names(specDF)=="1200")]))
infs
specDF <- specDF[-infs,]

# No spectra id
nas <- which(is.na(specDF$SpecID))
nas
specDF<- specDF[-nas,]

# invalid SpecID
nos<- which(specDF$specID == 0)
nos
specDF<- specDF[-nos,]

#Negative values
negs <- which(specDF$`350`<0)
negs
specDF<- specDF[-negs,]

##### Preprocess the NIR Spectra and do PCA ####


specRecs <- getAllSpectraFromDB()
specDF <- makeSpectaDFfromDBRecords(dbRecords=specRecs)



preNIR <- pre_proc_nir(specDF[,-c(1)],11,2,0)
PCA <- prcomp(preNIR, center = T, scale. = T)

cumvar<-cumsum(PCA$sdev^2 / sum(PCA$sdev^2))
pc.index<-min(which(cumvar>0.95))
pc.index = 10
head(PCA$x[, 1:pc.index])

oPCA <- data.frame(SpecID=specDF$SpecID, DateApplied=now(), Spectra_type='visNIR',  PCA$x)
head(oPCA[, 1:13])
tail(oPCA[, 1:10])


saveRDS(oPCA, 'C:/Projects/Spectra/workflowTesting/NIR_PCA.rds')   ####          These are what would be stored in the DB
saveRDS(oPCA[, 1:10], 'C:/Projects/Spectra/workflowTesting/NIR_PCA_1_to_10.rds')

###.................................... ####
#####   Spectra selection process from here on  #######




###.####
###.####
###.................................... ####
#### Extraneous Code ##########


which(is.na(Banjo))

rawSpecFile <- fromJSON(specLib$spectra_json_obj[[1]])


df <- fromJSON(jsn)
spec <- Spectra(wl = df$wavenumber , nir = df$intensity, id = 'ID1', units = "nm")
fnc <- soilspec.format::getTransformationFunction('pre_proc_nir')
tspec <- fnc(spec=spec, args = ParameterValues )
odf <- data.frame(wavenumber=tspec@wl, intensity=tspec@nir[1,], row.names=NULL)







SpecByLabResults = doQuery(tcon, sql)
idxs <- which(SpecByLabResults$labm_code == '4A1')
specIds <- SpecByLabResults[idxs,]$spectra_id
idxs <- which(specLib$spectra_id %in% specIds)
selSpectraRecs <- specLib[idxs,]


selSpectraRecs$spectra_json_obj[[1]]
