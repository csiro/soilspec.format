![example workflow](https://github.com/CSIRO-Soils/soilspec.format/actions/workflows/test.yml/badge.svg)

# Overview
soilspec.format is a package for extracting R data frames and
R metadata lists from soil spectra of various formats, currently:

* Nicolet (.spa)
* Bruker Opus Binary (.0)
* Thermo (.spc)
* ASD (.asd)

# Install

	# need devtools and Rcpp packages
	install.packages(c("devtools", "Rcpp"))
	 
	# need opusreader package
	devtools::install_github("pierreroudier/opusreader")
	 
	# install package from GitHub where "..." is a GitHub Personal Access Token
	devtools::install_github("CSIRO-Soils/soilspec.format", auth_token = "...")
	
# Example Usage

  > path <- soilspec.format::bruker.opus.binary.file.path()
  
  > result <- soilspec.format::read.soilspec(path)
  
  > names(result)
  [1] "status"        "mode"          "is.descending" "origin"        "type"          "data"          "metadata"     
  > result$status
  [1] 0
  
  > head(result$data)
    wavenumber intensity
  1   7498.200 0.1366372
  2   7496.770 0.1355442
  3   7495.341 0.1333837
  4   7493.911 0.1317485
  5   7492.481 0.1320595
  6   7491.052 0.1344453
  
  > result$metadata$unique_id
  [1] "DECCW257_2021-02-05 06:34:20"
  
  > result$metadata$sample_name
  [1] "DECCW257;Calibration;13;Ryan;Waite;;SOC_Calibration;Fine ground samples"
  
  > result$metadata$laser_wn
  [1] 11711.2
  
  > plot(result$data, type="l")
