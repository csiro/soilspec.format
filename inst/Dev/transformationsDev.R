library(spectacles)
library(soilspec.format)
library(tidyverse)
library(signal)

data('australia')
class(australia)
big.head(australia)
summary(australia)

spectra(australia) <- sr_no ~ ... ~ 350:2500
str(australia)

plot(australia)
plot_stack(australia)
plot_summary(australia)

m <- aggregate_spectra(australia, fun = mean)
plot(m)

summary(australia)
dim(australia)


data("australia")
spc <- australia
plot(spc)
plot_stack(spc)

path <- hone.lab.red.file.path()
hone <- read.soilspec(path)

ts <- pre_proc_nir(spc,n=11,p=2,m=0)
summary(ts)
str(ts)
plot(ts[1,])
big.head(ts)

pre_proc_nir <- function(spc,n=11,p=2,m=0){

  spc_abs <- data.frame(log(1/spc)) #absorbance


  spectra(spc_abs) <- ~350:2500

  nir_p <- spc_abs %>%

    spectacles::cut(wl =450:2450) %>%

    apply_spectra(sgolayfilt, n=n, p=p, m=m) %>%

    apply_spectra(snv)



  return(nir_p@nir)

}


function(args){

  expr <- 'a=5;b=2;c=7'
  parsed <- parse(text = args)
  typeof(parsed)
  eval(parsed)

  c
}



expr <- '5 * 2 + 44'
# calling the parse() function
parsed <- parse(text = expr)
# obtaining the object type
typeof(parsed)
# evaluating the parsed object
eval(parsed)



t1(x=2, y=3, z=4, T=8)




ts <- transformationFunctions[['pre_proc_nir']](spc, args = 'n=11;p=2;m=0;')
plot(ts[1,], type='l')
plot_stack(spc)
as.list(args(transformationFunctions[['pre_proc_nir']]))


transformationFunctions <-  c(
  pre_proc_nir=transform_Pre_proc_NIR,
  pca=transform_PCA

)


args <- 'n=11;p=2;m=0'
transform_Pre_proc_NIR(spc, args)

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

  return("Hello hello")

}







