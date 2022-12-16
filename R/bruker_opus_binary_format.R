# Bruker Opus binary format, from Bruker MIR instrument
#
# See:
# - https://github.com/pierreroudier/opusreader
# - https://github.com/spectral-cockpit/opusreader2
# - https://www.bruker.com/en/products-and-solutions/infrared-and-raman/ft-ir-routine-spectrometer/what-is-ft-ir-spectroscopy.html

BrukerOpusBinary <- R6::R6Class("BrukerOpusBinary",
  inherit = SpectrumFormat,

  public = list(
    initialize = function() {
      super$initialize(origin = "Bruker",
                       type_name = "MIR",
                       suffix = ".0")
    },

    read = function(path) {
      spec.df <- NULL
      meta.list <- NULL
      status <- 4
      mode <- NULL

      out <- tryCatch({
        # metadata via opusreader
        spec.data <- opusreader::opus_read(path)
        mode <- spec.data$metadata$result_spc
        meta.list <- as.list(spec.data$metadata)

        # data via opusreader2
        spec2.data <- opusreader2::read_opus_file(path)
        spec.df <- data.frame(wavenumber=spec2.data$spec$wavenumbers,
                              intensity=spec2.data$spec$data[1:ncol(spec2.data$spec$data)])

        status <- 0
      },
      error=function(cond) {
      },
      warning=function(cond) {
      },
      finally={
      })

      super$create.result(status, mode, spec.df, meta.list)
    }
  )
)
