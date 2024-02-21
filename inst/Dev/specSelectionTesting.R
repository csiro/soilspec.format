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



###### Now we need to select out spectra with required chem data  #####

oPCA <- readRDS( 'C:/Projects/Spectra/workflowTesting/NIR_PCA_1_to_10.rds')

att <- '4A1'
SpecType <- 'visNIR'

sql <- paste0("SELECT o.agency_code, o.proj_code, o.s_id, o.o_id, lr.h_no, lr.samp_no, lr.labr_no, lr.labm_code, o.o_latitude_GDA94, o.o_longitude_GDA94, s.samp_lower_depth, s.samp_upper_depth, o.o_date_desc, lm.LABM_NAME, lr.labr_value, sp.spectra_id, sp.spectra_type, sp.method_id,
             sp.spectra_source_file_name, sp.spectra_mode, sp.spectra_wavesignature_units
FROM   dbo.OBSERVATIONS AS o INNER JOIN
             dbo.SAMPLES AS s ON s.agency_code = o.agency_code AND s.proj_code = o.proj_code AND s.s_id = o.s_id AND s.o_id = o.o_id INNER JOIN
             dbo.LAB_RESULTS AS lr ON lr.agency_code = s.agency_code AND lr.proj_code = s.proj_code AND lr.s_id = s.s_id AND lr.o_id = s.o_id AND lr.h_no = s.h_no AND lr.samp_no = s.samp_no INNER JOIN
             dbo.LAB_METHODS AS lm ON lm.LABM_CODE = lr.labm_code INNER JOIN
             dbo.SPECTRA AS sp ON sp.agency_code = s.agency_code AND sp.proj_code = s.proj_code AND sp.s_id = s.s_id AND sp.o_id = s.o_id AND sp.h_no = s.h_no AND sp.samp_no = s.samp_no
WHERE (lr.labm_code = N'", att, "') and (sp.spectra_type = '", SpecType, "')")


phSpec <- doQuery(tcon, sql)

specIds <- phSpec$spectra_id
head(specIds)
length(specIds)

idxs <- which(oPCA$SpecID %in% specIds)
selectPop <- oPCA[idxs,]

mySpecPath <- 'C:/Projects/Spectra/Selection/Materials/Test_Set_B.RDS'
sampleSpec <- readRDS(mySpecPath)
mySpec <- as.data.frame(sampleSpec[1:20,])
mySpec[1:10, 1:10]
### Some weird stuff going on with data types
y <- mySpec[, c(-1)]
x <- data.frame(sapply(y, function(x) as.numeric(as.character(x))))

sapply(x, class)

SAPI_ChullNew(Local_Spectra=x, PcaPopulation=selectPop, npoints=100, bufferFraction = 0.2)



