#############################################################################
#####       Author : Ross Searle and James Moloney - mostly James       #####
#####       Date :  Mon Feb 12 10:54:55 2024                            #####
#####       Purpose : A series of functions using different approaches  #####
#####                 to select a subset of spectra from the National   #####
#####                 spectral library with signatures similar to the   #####
#####                 provided spectra set.                             #####
#####       Comments :                                                  #####
#############################################################################





grow_polygon_along_centroid <- function(vertices, scale_factor) {
  # Find the centroid of the polygon
  centroid <- apply(vertices, 2, mean)

  # Calculate vectors from the centroid to each vertex
  vectors_to_centroid <- vertices - centroid

  # Scale each vector
  scaled_vectors <- vectors_to_centroid * scale_factor

  # Calculate new vertices by adding scaled vectors to the centroid
  new_vertices <- centroid + scaled_vectors

  # Connect the new vertices to form the grown polygon
  grown_polygon <- rbind(new_vertices, new_vertices[1, ])

  return(grown_polygon)
}


SAPI_Chull <- function(Local_Spectra_path, npoints=100, bufferFraction = 0.2) {
  # Read data frames from RDS files
  #this section should parse a collection of selected spectra from json,
  #at the moment, im just sorcing a .rds file in the materials folder
  #
  LocalSpec <- as.data.frame(readRDS(Local_Spectra_path))[1:200,]
  numeric_col_names <- grep("^\\d+$", names(LocalSpec), value = TRUE)

  for(i in 2:ncol(LocalSpec)) {
    LocalSpec[,i] <- as.numeric(LocalSpec[,i])
  }



  #insert metadata or condition check here to identify which ""AR"" dataset and projection to source. this is probably passed to the api as an argument.
  Projection <- readRDS(projectionsPath)


  Scaled_Loc <- scale(LocalSpec[,numeric_col_names], center = Projection$Center, scale = Projection$Scale)

  #grab projected national data, and the indexed specIDs
  specIDs <- readRDS(nationalSpecLibPath)[,2]

  dfA <- readRDS(projectedNatPath)
  dfB <- as.data.frame(as.matrix(Scaled_Loc)%*%Projection$Rotations)
  # Extract coordinates
  coordinatesA <- dfA[, 1:3]
  coordinatesB <- dfB[, 1:3]

  # Calculate the convex hull of DataFrame B
  convexHullB <- convhulln(coordinatesB)

  # Buffer out the convex hull by 1/5th of its magnitude
  hullVertices <- coordinatesB[convexHullB, ]

  bufferedHullVertices <- grow_polygon_along_centroid(hullVertices,1+bufferFraction)


  # Buffer out the hull by 1+x th of its magnitude in each dimension. this is not currently doing this, its just increasing the magnitude, need to grow along normals.
  # bufferedHullVertices <- hullVertices + (1+bufferFraction) * hullVertices


  # Use convex hull computation for n dimensions from the geometry package

  # Calculate the convex hull of the buffered vertices
  bufferedConvexHullB <- convhulln(bufferedHullVertices)
  str(bufferedConvexHullB)

  #the convex hull is described as a series of points within the original dataset, to make the overlay work, we need to add the vertexes referenced to the top of the projected national set.
  Comb_coords <- rbind(bufferedHullVertices,coordinatesA)

  point_inside_hull <- which(inhulln(bufferedConvexHullB,p = as.matrix(Comb_coords)))
  point_inside_hull <- which(point_inside_hull>nrow(bufferedHullVertices))

  Selected <- clhs::clhs(dfA,size = npoints,use.cpp = T,iter=50000)


  return(specIDs[Selected])
}


SAPI_EmptyPlaceholder <- function(Local_Spectra_path, npoints=100){

  return('Not Implemented')
}


##### when you add a selection function into the code above, you need to add the required info about it in the code below  #####

### A named list of the available Selection methods
availableSelectionFunctions <-
  c(
    convexHull = SAPI_Chull,
    empty = SAPI_EmptyPlaceholder
  )


#' Return a spectra selection function.
#' @export
#' @param spectraSelectionName Name of spectra selection function to return
#' @return A spectra selection function
#' @examples getSelectionFunction('spectraSelectionName')


getSelectionFunction <- function(selectionName){
  return(availableSelectionFunctions[selectionName])
}


#' Return information about the available spectral selection functions.
#' @export
#' @param type Transformation types to return  c('Preprocessing', 'Harmonisation')
#' @return A dataframe of spectral selection information
#' @examples getSpectraSelectionInfo()

getSpectraSelectionInfo <- function(){

  Name <- c('convexHull', 'empty')
  CreationDate <- c('12/02/2024', '12/02/2024')
  Author <- c('James Moloney', 'Ross Searle')
  Description <- c('Uses a convex hull in projected space to select spectra subset', 'Place Holder')
  RequiredParameters = c( c('npoints=100; bufferFraction=0.2'), c('None'))
  Code <- c(paste0(capture.output(print(SAPI_Chull)), collapse="\n"),
            paste0(capture.output(print(SAPI_EmptyPlaceholder)), collapse="\n")
  )

  df <- data.frame(Name, Author, CreationDate, Description, RequiredParameters, Code)
  return(df)

}









