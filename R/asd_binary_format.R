# ASD binary format, e.g. for vis-NIR contact probe

ASDBinary <- R6::R6Class("ASDBinary",
  inherit = soilspec.format::SpectrumFormat,

  public = list(
    initialize = function() {
      super$initialize(origin = "ASD",
                       type_name = "NIR",
                       suffix = ".asd")
    },

    read = function(path) {
      spec.df <- NULL
      meta.df <- NULL
      status <- 1
      mode <- NULL
      units <- NULL

      out <- tryCatch({
        spec.data <- asdreader::get_spectra(path)
        spec.data <- t(spec.data)

        # TODO: could instead use a tidyverse approach here;
        #       also: do we want to return data frames or
        #       data tables?
        table <- cbind(x=rownames(spec.data), y=spec.data[,])
        spec.df <- data.frame(wavenumber=as.double(table[,1]),
                              intensity=as.double(table[,2]))
        rownames(spec.df) <- NULL

        meta.df <- as.data.frame(asdreader::get_metadata(path))

        mode <- as.character(meta.df$data_type)
        units <- "" # TODO!
        status <- 0
      },
      error=function(cond) {
      },
      warning=function(cond) {
      },
      finally={
      })

      super$create.result(status, mode, units, spec.df, meta.df)
    }
  )
)
