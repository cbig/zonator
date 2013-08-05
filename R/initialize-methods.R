# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomäki <joona.lehtomaki@gmai.com>. All rights 
# reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' Create an instance of the Zproject class using new/initialize.
#'
#' @param root Character string path to the root of the project (must exist).
#'
#' @seealso \code{\link{initialize}}
#'
#' @keywords internal
#'
#' @rdname initialize-methods
#' @author Joona Lehtomaki <joona.lehtomaki@@gmail.com>
#' 
setMethod("initialize", "Zproject", function(.Object, root) {
  
  if (!file.exists(root)) {
    stop(paste0("Root folder ", root, " not found")) 
  } else {
    .Object@root <- root
  }
  
  variants <- list()
  
  # List all the bat-files
  bat.files <- list.files(root, ".bat$", full.names=TRUE)
  
  for (bat.file in bat.files) {
    
    variants[bat.file] <- new("Zvariant", bat.file=bat.file)
  }
  
  .Object@variants <- variants 
  
  .Object
})

#' Create an instance of the Zvariant class using new/initialize.
#'
#' @param name Character string naming the variant.
#' @param bat.file Zonation specific batch (.bat) file to read the 
#'   variant specifics from (must exist).
#'
#' @seealso \code{\link{initialize}}
#'
#' @keywords internal
#'
#' @rdname initialize-methods
#' @author Joona Lehtomaki <joona.lehtomaki@@gmail.com>
#' 

setMethod("initialize", "Zvariant", function(.Object, name=NULL, bat.file) {
  
  if (!file.exists(bat.file)) {
    stop(paste0("Variant .bat-file does not exist: ", bat.file))
  }
  
  if (is.null(name)) {
    # If no name is provided, use the name of the bat-file (without the 
    # extension)
    .Object@name <- strsplit(".", basename(bat.file))[[1]]
  } else {
    .Object@name <- name
  }
  .Object@bat.file <- bat.file
  # Read the content of the bat file
  .Object@call.params <- read.bat(bat.file)
  
  results <- list()
  
  # bat-file's existence has already been verified, try to get the output. 
  # bat-file includes a template for an output file, but we're more  interested
  # in the directory where that file resides in.
  output.folder <- dirname(.Object@call.params[["output.file"]]) 
  
  if (!is.null(output.folder)) {
    
    .get.file <- function(output.folder, x) {
      
      target <- list.files(output.folder, pattern=x, full.names=TRUE)
      if (length(target) == 0) {
        return(NA)
      } else if (length(target) == 1) {
        return(target)
      } else {
        warning(paste("More matches than 1 found for", x, "using only the first"))
        return(target[1])
      }
    }
    # Curves file is named *.curves.txt. NOTE: if the file does not exist,
    # returns NA.
    curve.file <- .get.file(output.folder, "\\.curves\\.txt")
    results[["curves"]] <- read.curves(curve.file)
    
    # Group curves file is named *.grp_curves.txt. NOTE: if the file does not 
    # exist, returns NA.
    grp.curve.file <- .get.file(output.folder, "\\.grp_curves\\.txt")
    results[["grp.curves"]] <- read.grp.curves(grp.curve.file)
    
    # Rank raster file is named *.rank.*
    results["rank.raster.file"] <- .get.file(output.folder, "\\.rank\\.")
    
    .Object@results <- results
  }
  
  .Object
})
