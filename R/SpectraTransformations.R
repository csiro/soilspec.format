######################################################################
#####       Author : Ross Searle                                 #####
#####       Date :  Tue Jan 23 15:15:54 2024                     #####
#####       Purpose : Standard spectra transformation functions  #####
#####       Comments :                                           #####
######################################################################


#### Currently this is just dev code supplied by James Moloney for dev purposes
transform_Pre_proc_NIR <- function(spec, args){

  args <- str_replace_all(args, 'eq', '=')
  parsed <- parse(text = args)
  typeof(parsed)
  eval(parsed)

spec <- apply_spectra(spec, function(x) log(1/x))#### change to absorbance
  ###spc_abs <- data.frame(log(1/spec)) #absorbance


 # spectra(spc_abs) <- ~350:2500
  nir_p <- spec %>%
    spectacles::cut(wl =450:2450) %>%
  spectacles::apply_spectra(sgolayfilt, n=n, p=p, m=m) %>%
  spectacles::apply_spectra(snv)
  return(nir_p)

}


####  Just another dummy transformation for dev purposes
transform_PCA <- function(){
  ### This is just a placeholder
  return("Hello hello")
}

### A named list of the available transformations
transformationFunctions <-
  c(
      pre_proc_nir = transform_Pre_proc_NIR,
      pca = transform_PCA
)


#' Return a transformation function.
#' @export
#' @param transformationName Transformation name of function to return
#' @return A spectra transformation function
#' @examples getTransformationFunction('transformationName')


getTransformationFunction <- function(transformationName){
 f <- transformationFunctions[[transformationName]]
 return(f)
}


#' Return information about the available spectral transformations.
#' @export
#' @param type Transformation types to return  c('Preprocessing', 'Harmonisation')
#' @return A dataframe of spectral transformation information
#' @examples getTransformationInfo('Preprocessing')

getTransformationInfo <- function(type=NULL){

  Name <- c('pre_proc_nir', 'pca')
  CreationDate <- c('23/01/2024', '23/01/2024')
  Author <- c('Ross Searle', 'Ross Searle')
  Description <- c('NIR Preprocessing demo from JM', 'Place Holder')
  Type  <- c('Preprocessing', 'Harmonisation')
  RequiredParameters = c( c('m=notsure; n=filterSize; p=something'), c('None'))
  Code <- c(paste0(capture.output(print(transform_Pre_proc_NIR)), collapse="\n"),
            paste0(capture.output(print(transform_PCA)), collapse="\n")
            )

  df <- data.frame(Name, Author, CreationDate, Type, Description, RequiredParameters, Code)
  return(df)

}






