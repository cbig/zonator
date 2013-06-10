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

setClass("Zproject", representation(root = "character", variants = "list"))

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

check_variant <- function(object) {
  errors <- character()
  warnings <- character()
  
  # Check the batch file call parameters
  call.params <- object@call.params
  
  if (is.null(check.path(call.params[["dat.file"]]))) {
    msg <- paste0("dat-file ", call.params[["dat.file"]], " cannot be found")
    errors <- c(errors, msg)
  }
  
  if (is.null(check.path(call.params[["spp.file"]]))) {
    msg <- paste0("spp-file ", call.params[["spp.file"]], " cannot be found")
    errors <- c(errors, msg)
  }
  
  if (is.null(check.path(call.params[["dat.file"]]))) {
    msg <- paste0("dat-file ", call.params[["dat.file"]], " cannot be found")
    errors <- c(errors, msg)
  }
  
  if (is.null(check.path(call.params[["output.file"]]))) {
    msg <- paste0("Output location ", call.params[["output.file"]], 
                  " does not seem to exist.")
    warnings <- c(warnings, msg)
  }
  
  if (call.params[["uc.alpha"]] < 0) {
    # FIXME: is there an upper bound?
    msg <- paste0("Uncertainty parameter alpha cannot be negative: ", 
                  call.params[["uc.alpha"]])
    errors <- c(errors, msg)
  }
  
  if (!call.params[["ds.switch"]] %in% c(0, 1)) {
    msg <- paste0("Distribution smoothing switch must be 0 or 1: ", 
                  call.params[["ds.switch"]])
    errors <- c(errors, msg)
  }
  
  if (!call.params[["close.window"]] %in% c(0, 1)) {
    msg <- paste0("Close window switch must be 0 or 1: ", 
                  call.params[["close.window"]])
    errors <- c(errors, msg)
  }
  
  if (length(warnings) != 0) {
    for (.warning in warnings) {
      warning(.warning)
    }
  }
  
  if (length(errors) == 0) TRUE else errors
}

setClass("Zvariant", representation(name = "character", bat.file = "character",
                                    call.params = "list", results = "list"),
         validity = check_variant)

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
    # Curves file is named *.curves.txt
    results[["curves"]] <- read.curves(.get.file(output.folder, 
                                                 "\\.curves\\.txt"))
    
    # Group curves file is named *.grp_curves.txt
    results[["grp.curves"]] <- read.grp.curves(.get.file(output.folder, 
                                                        "\\.grp_curves\\.txt"))
    
    # Rank raster file is named *.rank.*
    results["rank.raster.file"] <- .get.file(output.folder, "\\.rank\\.")
    
    .Object@results <- results
  }
  
  .Object
})

setMethod("plot", signature=c(x="Zvariant"), function(x, group=FALSE, ...) {
  if (group) {
    plot.z.grp.curves(x@results[["grp.curves"]], ...)
  } else {
    plot.z.curves(x@results[["curves"]], ...)
  }
})