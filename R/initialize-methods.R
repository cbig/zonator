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
    variant <- new("Zvariant", bat.file=bat.file)
    variants[variant@name] <- variant
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
  
  # Set the name ###############################################################
  
  if (is.null(name)) {
    # If no name is provided, use the name of the bat-file (without the 
    # extension)
    .Object@name <- strsplit(basename(bat.file), "\\.", )[[1]][1]
  } else {
    .Object@name <- name
  }
  .Object@bat.file <- bat.file
  # Read the content of the bat file
  call.params <- read_bat(bat.file)
  .Object@call.params <- call.params
  
  # NOTE: dat-file and spp-file existence has already been checked by zvariant
  # initializer checker function
  
  # dat-file content ###########################################################
  .Object@dat.data <- read_ini(call.params$dat.file)
  
  # spp-file content ###########################################################
  spp.data <- read_spp(call.params$spp.file)

  # Extract spp feature "names" from the raster file path
  spp.data$name <- basename(tools::file_path_sans_ext(spp.data$filepath))
  
  .Object@spp.data <- spp.data
  
  # groups content #############################################################
  
  # First we need to define whether groups are 1) used, and 2) available. 
  settings <- .Object@dat.data$Settings
  if ("use_groups" %in% names(settings)) {
    if (settings$use_groups == 1) {
      if ("groups_file" %in% names(settings)) {
        .Object@groups <- read_groups(file.path(dirname(bat.file), 
                                                settings$groups_file))
      }
    }
  }
  
  # results ####################################################################
  
  results <- list()
  
  # bat-file's existence has already been verified, try to get the output. 
  # Consider results done if you find 
  output.folder <- .Object@call.params$output.folder
  if (!is.null(output.folder)) {
    
    get_file <- function(output.folder, x) {
      
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
    
    # Extract the 'last modified' information from run info file
    run.info.file <- get_file(output.folder, "\\.run_info\\.txt")
    if (!is.na(run.info.file)) {
      results[["modified"]] <- file.info(run.info.file)$mtime
    } else {
      results[["modified"]] <- NA
    }
    
    # Curves file is named *.curves.txt. NOTE: if the file does not exist,
    # returns NA.
    curve.file <- get_file(output.folder, "\\.curves\\.txt")
    results[["curves"]] <- read_curves(curve.file)
    
    # Group curves file is named *.grp_curves.txt. NOTE: if the file does not 
    # exist, returns NA.
    grp.curve.file <- get_file(output.folder, "\\.grp_curves\\.txt")
    results[["grp.curves"]] <- read_grp_curves(grp.curve.file)
    
    # Rank raster file is named *.rank.*
    results["rank.raster.file"] <- get_file(output.folder, "\\.rank\\.")
    
    .Object@results <- results
  }
  
  .Object
})
