# Thermo galactic spc format

# TODO: obtain mode per file

ThermoSpc <- R6::R6Class("ThermoSpc",
  inherit = soilspec.format::SpectrumFormat,
  
  public = list(
    initialize = function() {
      super$initialize(origin = "Thermo Galactic",
                       type_name = "IR/Raman/UV/VIS",
                       suffix = ".spc")
    },
    
    read = function(path) {
      spec.df <- NULL
      meta.list <- NULL
      status <- 1
      mode <- NULL
      units <- NULL
      
      out <- tryCatch({
        spec.data <- hyperSpec::read.spc(path)
        mode <- "?" # TODO: obtain from metadata?
        units <- "?" # TODO: (e.g. hyperSpec object shows cm-1)
        spec.df <- data.frame(wavenumber=as.double(names(spec.data$spc[,])),
                              intensity=as.double(spec.data$spc[,]))
        meta.list <- list()
        status <- 0
      },
      error=function(cond) {
      },
      warning=function(cond) {
      },
      finally={
      })
      
      super$create.result(status, mode, units, spec.df, meta.list)
    }
  )
)
