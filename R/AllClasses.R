# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomäki <joona.lehtomaki@gmai.com>. All rights 
# reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# Zonation project ------------------------------------------------------------

setClass("Zproject", representation(root = "character",
                                    variants = "list"))

setMethod("initialize", "Zproject", function(.Object, root) {

  if (!file.exists(root)) {
    stop(paste0("Root folder ", root, " not found")) 
  } else {
    .Object@root <- root
  }

  variants <- list()
  
  # All the folders that 1) start with a number and 2) have a subfolder 
  # "output" are cosidered to be variants
  folders <- list.dirs(root, recursive=FALSE)
  
  for (folder in folders) {
    # Get the leaf folder
    variant.folder <- tail(unlist(strsplit(folder, "/")), n=1)
    # A variant folder must be a directory starting with numbers and it
    # must have a subfolder named "output"
    if (grepl("^[0-9]+", variant.folder) & file.exists(file.path(folder, 
                                                                 "output"))) {
      variants[variant.folder] <- new("Zvariant", name=variant.folder,
                                      root=folder)
    } else {
      next
    } 
  }
  .Object@variants <- variants 
  
  .Object
})

#' getVariant
#' Get a specified variant in a Zonation project
#'
#' @param x Zproject object
#' @param index int or string index defining the variant required
#'
#' @return Zvariant object
#' 
#' @docType methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export

setMethod("getVariant", c("Zproject", "ANY"), function(x, index) {
    return(x@variants[[index]])
  }
)

#' nvariants
#' Get the number of variants included in a Zonation project
#'
#' @param x Zproject object
#'
#' @return int number of variants
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export

setMethod("nvariants", c("Zproject"), function(x) {
    return(length(x@variants))
  }
)

setMethod("names", "Zproject", function(x) {
    return(names(x@variants))
  }
)

#' open.dir
#' Open the directory of a Zproject using the system file browser.
#' 
#' Currently support Windows Explorer (Windows) amd Dolphin (Linux/KDE)
#'
#' @param x object
#'
#' @return invisible
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export

setMethod("opendir", c("Zproject"), function(object) {
    invisible(open.dir(object@root))
  }
)

# Zonation variant --------------------------------------------------------

setClass("Zvariant", representation(name = "character",
                                    root = "character",
                                    results = "list"))

setMethod("initialize", "Zvariant", function(.Object, name, root) {

  .Object@name <- name
  .Object@root <- root
  
  results <- list()
  
  if (file.exists(root)) {
    # Try if there is a subfolder named "output", if not, let's try the root
    output.folder <- file.path(root, "output") 
    if (!file.exists(output.folder)) {
      output.folder <- root
    }
    
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
    
    # Curves file is named *.curves.txt
    results[["curves"]] <- read.curves(.get.file(output.folder, 
                                                 "\\.curves\\.txt"))
    
    # Group curves file is named *.grp_curves.txt
    results[["grp.curves"]] <- read.grp.curves(.get.file(output.folder, 
                                                        "\\.grp_curves\\.txt"))
    
    # Rank raster file is named *.rank.*
    results["rank.raster.file"] <- .get.file(output.folder, "\\.rank\\.")
    
  } else {
    stop(paste0("Cannot create variant "), name, ": path ", root, " does not exist")
  }
  
  .Object@results <- results
  
  .Object
})

setMethod("plot", signature=c(x="Zvariant"), function(x, group=FALSE, ...) {
  if (group) {
    plot.z.grp.curves(x@results[["grp.curves"]], ...)
  } else {
    plot.z.curves(x@results[["curves"]], ...)
  }
})