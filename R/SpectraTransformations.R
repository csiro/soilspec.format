######################################################################
#####       Author : Ross Searle                                 #####
#####       Date :  Tue Jan 23 15:15:54 2024                     #####
#####       Purpose : Standard spectra transformation functions  #####
#####       Comments :                                           #####
######################################################################

transform_Pre_proc_NIR <- function(spec, args){

  parsed <- parse(text = args)
  typeof(parsed)
  eval(parsed)

  spc_abs <- data.frame(log(1/spc)) #absorbance
  spectra(spc_abs) <- ~350:2500
  nir_p <- spc_abs %>%
    spectacles::cut(wl =450:2450) %>%
    apply_spectra(sgolayfilt, n=n, p=p, m=m) %>%
    apply_spectra(snv)
  return(nir_p@nir)

}

transform_PCA <- function(){
  ### This is just a placeholder
  return("Hello hello")
}


transformationFunctions <-  c(
  pre_proc_nir=transform_Pre_proc_NIR,
  pca=transform_PCA
)


#' Return a transformation function.
#' @export
#' @param transformationName Transformation name of function to return
#' @return A spectra transformation function


getTransformationFunction <- function(transformationName){
 f <- transformationFunctions[[transformationName]]
 return(f)
}


#' Return information about the available spectral transformations.
#' @export
#' @param type Transformation types to return
#' @return A dataframe of spectral transformation information

getTransformationInfo <- function(type=NULL){

  Name <- c('pre_proc_nir', 'pca')
  CreationDate <- c('23/01/2024', '23/01/2024')
  Author <- c('Ross Searle', 'Ross Searle')
  Description <- c('NIR Preprocessing demo from JM', 'Place Holder')
  Type  <- c('Preprocessing', 'Harmonisation')
  Code <- c(paste0(capture.output(print(transform_Pre_proc_NIR)), collapse="\n"),
            paste0(capture.output(print(transform_PCA)), collapse="\n")
            )

  df <- data.frame(Name, Author, CreationDate, Type, Description, Code)
  return(df)

}






