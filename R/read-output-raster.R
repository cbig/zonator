# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomäki <joona.lehtomaki@gmai.com>. All rights 
# reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' Read Zonation-specific raster result files.
#'
#' Input rasters are given as raster names (i.e. without the raster file 
#' extension). Additional root (folder) path and file extension can be provided
#' to construct the full paths.
#'
#' @param \code{rasters} Character vector of raster names.
#' @param \code{path} Character string indication an optional root path that is
#'   prepended to each \code{rasters} names.
#' @param \code{format} Character string indicating the raster format used 
#'   (i.e. the file extension).
#'
#' @return A \code{\link{RasterStack}} object of result rasters.
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export
#' 
read.result.rasters <- function(rasters, path=NULL, format=NULL) {
  if (is.null(format)) {
    ext <- ".rank.asc"
  } else {
    ext <- format
  }
  
  results <- stack(sapply(rasters, function(x){
                                      raster(file.path(path, x, "output",
                                                       paste("result_", x, ext, 
                                                             sep="")))}, 
                          USE.NAMES=F))
  return(results)
}