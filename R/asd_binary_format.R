# ASD binary format, e.g. for vis-NIR contact probe

ASDBinary <- R6::R6Class("ASDBinary",
  inherit = SpectrumFormat,

  public = list(
    initialize = function() {
      super$initialize(origin = "ASD",
                       type_name = "visNIR", # TODO: true for all ASD files?
                       suffix = ".asd")
    },

    read = function(path) {
      spec.df <- NULL
      meta.list <- NULL
      status <- 4
      mode <- NULL

      out <- tryCatch({
        spec.data <- asdreader::get_spectra(path)
        spec.data <- t(spec.data)

        table <- cbind(x=rownames(spec.data), y=spec.data[,])
        spec.df <- data.frame(wavenumber=as.double(table[,1]),
                              intensity=as.double(table[,2]))
        rownames(spec.df) <- NULL

        meta.list <- asdreader::get_metadata(path)

        mode <- as.character(meta.list$data_type)
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
