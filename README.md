<!--
![test workflow](https://github.com/csiro-internal/soilspec.format/actions/workflows/test.yml/badge.svg)
-->

# Overview
soilspec.format is a package for extracting data frames and
metadata lists from soil spectra of various formats, currently:

* Nicolet (.spa)
* Bruker Opus Binary (.0)
* Thermo (.spc)
* ASD (.asd)
* ASD SCO (.sco)
* Perkin Elmer (.sp) [PEPE and PE IR magic numbers]
* Hone Lab Red (.hlr)
* Hone Lab Red Reduced (.hlrr)
* Spectral Evolution (.sed)
* CSIRO SCANS (.scan)
* CSV (.csv)

The extracted data frames will always have the columns: "wavenumber" and "intensity".


# Install

	# need devtools and Rcpp packages
	install.packages(c("devtools", "Rcpp"))
	 
	# need opusreader and opusreader2 packages
	devtools::install_github("pierreroudier/opusreader")
	devtools::install_github("spectral-cockpit/opusreader2")

	# install package from GitHub
	devtools::install_github("csiro/soilspec.format")

  Under Windows, you will also need to install [Rtools](https://cran.r-project.org/bin/windows/Rtools)
  to match your R version.

	
# Example Usage
```
> library(soilspec.format)

> path <- asd.binary.file.path()

> result <- read.soilspec(path)

> names(result)
 [1] "status"                  "mode"                    "is.absorbance"           "is.reflectance"          "is.transmittance"       
 [6] "is.descending"           "origin"                  "type"                    "data"                    "standardised.metadata"  
[11] "all.instrument.metadata"   

> result$status
[1] 0

> result$is.descending
[1] FALSE

> result$is.reflectance
[1] TRUE

> result$mode
[1] "reflectance"

> head(result$data)
  wavenumber  intensity
1        350 0.05432920
2        351 0.04792801
3        352 0.05130503
4        353 0.05339854
5        354 0.04970677
6        355 0.05435767

> plot(result$data, type="l")

> result$standardised.metadata
$sample_id
[1] ""

$spectra_id
[1] ""

$spectra_source_file_name
[1] "example.asd"

$date_time
[1] "2017-06-21 10:33:49 ACST"

$response
[1] "reflectance"

$instrument_technology_type
[1] "visNIR"

$instrument_manufacturer
[1] "ASD"

$instrument
[1] "FieldSpec FR"

...

$bulb
[1] 0

$swir1_gain
[1] 569

$swir2_gain
[1] 1160

$swir1_offset
[1] 2180

$swir2_offset
[1] 2415

$splice1_wavelength
[1] 1000

$splice2_wavelength
[1] 1830

> result$all.instrument.metadata
$co
[1] "as7"

$comments
[1] ""

$when
[1] "2017-06-20 19:04:47 ACST"

$program_version
[1] "6.0"

$file_version
[1] "7.0"

$dc_corr
[1] TRUE
...
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
  suffix used for files of the format. In fact, it doesn't matter what the prefix
  of the file is but the suffix does matter (unlike the empty file referred to in
  a step below). Ensure there are no IP concerns with making the file available
  in the library.

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
