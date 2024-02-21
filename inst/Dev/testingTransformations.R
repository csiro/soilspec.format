library(DBI)
library(odbc)
library(jsonlite)
library(spectacles)
library(tidyverse)
library(signal)



doQuery <- function(con, sql){
  res <- dbSendQuery(con, sql)
  rows <- dbFetch(res)
  dbClearResult(res)
  return(rows)
}

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





tcon <- DBI::dbConnect(odbc::odbc(),
                       Driver   = "ODBC Driver 17 for SQL Server",
                       Server   = "sql-asris-stage.it.csiro.au",
                       Database = 'CCDB_SPECTRAv2',
                       Trusted_Connection = "yes")

tcon <- DBI::dbConnect(odbc::odbc(),
                       Driver   = "ODBC Driver 17 for SQL Server",
                       Server   = "sql-asris-stage.it.csiro.au",
                       Database = 'CCDB_SPECTRAv2',
                       UID = "SPECTRA_READ",
                       PWD = 'Cl@ncy0ffTh3Overfl0w'
                       )


  sql <- paste0("select * from SPECTRA where s_id = 'NSABHC0013'")

  sql <- paste0("SELECT COUNT(*) AS number_of_rows FROM SPECTRA")

  sql <- paste0("select spectra_id from SPECTRA")

  sql <- paste0("select * from SPECTRA")

  fdf = doQuery(tcon, sql)

  saveRDS(fdf, 'C:/Projects/Spectra/Selection/Materials/NationalLibraryDB.rds')

specLib <- readRDS('C:/Projects/Spectra/Selection/Materials/NationalLibraryDB.rds')

idxs <- which(fdf$labm_code == '4A1')
recs <- specLib[idxs,]







for (i in 1:nrow(s)) {
  rec <- s[i,]

 jsn <- rec$spectra_json_obj
 so <- fromJSON(jsn)
 spec <- Spectra(wl = so$wavesignatures , nir = so$values, id = rec$spectra_id)
 tspec <- transform_Pre_proc_NIR(spec=spec)
# plot(tspec)
 print(i)
}



recs <- s[1:100, ]$spectra_json_obj
so <- fromJSON(recs)
spec <- Spectra(wl = recs$spectra_file , nir = jo$values, id = rec$spectra_id[1:100])

'n=11;p=2;m=0;'


transform_Pre_proc_NIR <- function(spec, n=11, p=2, m=0){

  # parsed <- parse(text = args)
  # typeof(parsed)
  # eval(parsed)
  #
  spec <- apply_spectra(spec, function(x) log(1/x))#### change to absorbance
  ###spc_abs <- data.frame(log(1/spec)) #absorbance


  # spectra(spc_abs) <- ~350:2500
  nir_p <- spec %>%
    spectacles::cut(wl =450:2450) %>%
    spectacles::apply_spectra(sgolayfilt, n=n, p=p, m=m) %>%
    spectacles::apply_spectra(snv)
  return(nir_p)

}


