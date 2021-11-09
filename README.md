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
