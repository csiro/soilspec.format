![test workflow](https://github.com/CSIRO-Soils/soilspec.format/actions/workflows/test.yml/badge.svg)

# Overview
soilspec.format is a package for extracting R data frames and
R metadata lists from soil spectra of various formats, currently:

* Nicolet (.spa)
* Bruker Opus Binary (.0)
* Thermo (.spc)
* ASD (.asd)
* ASD SCO (.sco)
* Perkin Elmer (.sp) [PEPE and PE IR magic numbers]
* Hone Lab Red (.hlr)
* CSIRO SCANS (.scan)
* CSV (.csv)

The extracted data frames will always have the columns: "wavenumber" and "intensity".


# Install

	# need devtools and Rcpp packages
	install.packages(c("devtools", "Rcpp"))
	 
	# need opusreader and opusreader2 packages
	devtools::install_github("pierreroudier/opusreader")
	devtools::install_github("spectral-cockpit/opusreader2")

	# install package from GitHub where "..." is a GitHub Personal Access Token
	devtools::install_github("CSIRO-Soils/soilspec.format", auth_token = "...")

  Under Windows, you will also need to install [Rtools](https://cran.r-project.org/bin/windows/Rtools)
  to match your R version.

	
# Example Usage
  ```
  path <- soilspec.format::bruker.opus.binary.file.path()
  
  result <- soilspec.format::read.soilspec(path)
  
  [1] "status"        "mode"          "is.descending" "origin"        "type"          "data"          "metadata"     
 
  result$status
  [1] 0
  
  head(result$data)
    wavenumber intensity
  1   7498.200 0.1366372
  2   7496.770 0.1355442
  3   7495.341 0.1333837
  4   7493.911 0.1317485
  5   7492.481 0.1320595
  6   7491.052 0.1344453
  
  result$metadata$unique_id
  [1] "DECCW257_2021-02-05 06:34:20"
  
  result$metadata$sample_name
  [1] "DECCW257;Calibration;13;Ryan;Waite;;SOC_Calibration;Fine ground samples"
  
  result$metadata$laser_wn
  [1] 11711.2
  
  plot(result$data, type="l")
  ```

# Developers
## Install
Instead of using `devtools::install_github()` to install `soilspec.format` for
development purposes, use this command to install the package:
  
  `devtools::install()`

## Test
To run unit tests use this command:

  `devtools::test()`

## Add Support for a New Format
* Create a sub-class of `SpectrumFormat` (see `spectrum_format.R`) in the `R`
  directory.
  * Look at `R/*_format.R` files for examples.

* Near the top of `R/read_soilspec.R` add a mapping from a file suffix to
  an instance of the new ``SpectrumFormat` sub-class, e.g.

  `soilspec.readers[[".asd"]] <- ASDBinary$new()`

* Create a format-specific reader function in `R/read_soilspec.R` by starting
  with an example, e.g. `read.bruker.opus.binary`

* Add an example file in a sub-directory under `inst/extdata` named to reflect
  the file format. The file should start with `example` and end in the
  suffix used for files of the format. Ensure there are no IP concerns with
  making the file available in the library.

* Create a file path function in `R/file_paths.R` to access this file. This
  function can be used for testing and by users of the library.

* Add an empty file in `inst/extdata/Unknown` that starts with `example` and ends in the
  suffix associated with the format. This will be used with `common_read_test` (see below).

* In `tests/testthat/test_read_soilspec.R`, add a test case for the new format above the lines:
~~~
     #######################################
     # Add tests for new formats above ^^^ #
     #######################################
~~~
* See examples above those lines for other format reader tests that
  call `common_read_test`.

* Add a unit test for the sub-class of `SpectrumFormat` in a new file 
  under `tests/testthat`. Look at other format-specific test files
  there to get started, e.g. `test_bruker_opus_binary.R` or
  `test_asd_sco_binary.R`
