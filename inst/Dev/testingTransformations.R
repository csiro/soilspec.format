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

tcon <- DBI::dbConnect(odbc::odbc(),
                       Driver   = "ODBC Driver 17 for SQL Server",
                       Server   = "sql-asris-stage.it.csiro.au",
                       Database = 'CCDB_SPECTRAv2',
                       Trusted_Connection = "yes")


  sql <- paste0("select * from SPECTRA where s_id = 'NSABHC0013'")

  sql <- paste0("SELECT COUNT(*) AS number_of_rows FROM SPECTRA")

  sql <- paste0("select spectra_id from SPECTRA")

  sql <- paste0("select * from SPECTRA")

  fdf = doQuery(tcon, sql)

  saveRDS(fdf, 'c:/temp/spectraRecords.rds')

s <- readRDS('c:/temp/spectraRecords.rds')


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


