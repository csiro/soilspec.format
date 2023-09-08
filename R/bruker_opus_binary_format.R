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
      mode <- NULL

      status <- super$file_status(path)

      if (status == 0) {
        status <- 4

        out <- tryCatch({
        },
        error=function(cond) {
        },
        warning=function(cond) {
        },
        finally={
          # metadata via opusreader
          spec.data <- opusreader::opus_read(path)
          mode <- spec.data$metadata$result_spc
          meta.list <- as.list(spec.data$metadata)

          # data via opusreader2
          opus2 <- opusreader2::read_opus(path)
          fname <- names(opus2)[1]
          ab_no_atm_comp <- opus2[[fname]][["ab_no_atm_comp"]]
          intensities <- ab_no_atm_comp[["data"]]
          wavenumbers <- ab_no_atm_comp[["wavenumbers"]]
          spec.df <- data.frame(wavenumber=wavenumbers, intensity=unlist(as.list(intensities)))

          status <- 0
        })
      }

      super$create.result(status, mode, spec.df, meta.list)
    }
  )
)
