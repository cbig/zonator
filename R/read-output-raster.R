 
read.results <- function(rasters, path=NULL, format=NULL) {
  if (is.null(format)) {
    ext <- ".rank.asc"
  } else {
    ext <- format
  }
  
  results <- stack(sapply(rasters, function(x){named.raster(filepath=file.path(path,
                                                                               paste("result_", x, ext, sep="")),
                                                            name=x) }, 
                          USE.NAMES=F))
  return(results)
}

# Function for reading in Zonation result rasters in various formats

read.result.rasters <- function(rasters, path=NULL, format=NULL) {
  if (is.null(format)) {
    ext <- ".rank.asc"
  } else {
    ext <- format
  }
  
  results <- stack(sapply(rasters, function(x){raster(file.path(path, x, "output",
                                                                paste("result_", x, ext, sep=""))) }, 
                          USE.NAMES=F))
  return(results)
}