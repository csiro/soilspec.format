# Thermo galactic spc format

ThermoSpc <- R6::R6Class("ThermoSpc",
  inherit = SpectrumFormat,

  public = list(
    initialize = function() {
      super$initialize(origin = "Thermo Galactic",
                       type_name = "IR/Raman/UV/VIS",
                       suffix = ".spc")
    },

    read = function(path) {
      spec.df <- NULL
      meta.list <- NULL
      status <- 4
      mode <- NULL

      status <- super$file_status(path)

      ### raw spec file does not contain any metadata so just
      #   returning and empty standard metadata object for consistency
      stdmeta <- createStandardMetadataContainer()
      stdmeta[['spectra_wavesignature_units']] <- 'nm'

      if (status == 0) {
        status <- 4

        out <- tryCatch({
          spec.data <- hyperSpec::read.spc(path, log.txt = F, )
          spec.df <- data.frame(wavenumber=as.double(names(spec.data$spc[,])),
                                intensity=as.double(spec.data$spc[,]))
          status <- 0
        },
        error=function(cond) {
          status <- 2
        },
        warning=function(cond) {
          status <- 2
        },
        finally={
          # spec.data <- hyperSpec::read.spc(path, log.txt = F, )
          # spec.df <- data.frame(wavenumber=as.double(names(spec.data$spc[,])),
          #                       intensity=as.double(spec.data$spc[,]))
          # status <- 0
        })
      }

      super$create.result(status, mode, spec.df, meta.list, std_meta=stdmeta)
    }
  )
)
