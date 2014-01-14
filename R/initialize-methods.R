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
  bat.files <- list.files(root, ".bat$", full.names=TRUE, recursive=TRUE)
  
  for (bat.file in bat.files) {
    variant <- new("Zvariant", bat.file=bat.file)
    variants[variant@name] <- variant
  }
  
  .Object@variants <- variants 
  
  .Object
})

#' Create an instance of the Zresults class using new/initialize.
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
setMethod("initialize", "Zresults", function(.Object, root) {
  
  result.list <- list()
  
  # Find various result files from the root path which must exist
  if (!file.exists(root)) {
    warning("Results root path ", root, " does not exist")
    return(.Object)
  }
  
  .Object@root = root
  
  # Helper function
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
  run.info.file <- get_file(root, "\\.run_info\\.txt")
  if (!is.na(run.info.file)) {
    .Object@run.info <- run.info.file
    .Object@modified <- file.info(run.info.file)$mtime
  }
  
  # Curves file is named *.curves.txt. NOTE: if the file does not exist,
  # returns NA.
  curve.file <- get_file(root, "\\.curves\\.txt")
  if (!is.na(curve.file)) {
    .Object@curves <- read_curves(curve.file)
  }
    
  # Group curves file is named *.grp_curves.txt. NOTE: if the file does not 
  # exist, returns NA.
  grp.curve.file <- get_file(root, "\\.grp_curves\\.txt")
  if (!is.na(grp.curve.file)) {
    .Object@grp.curves <- read_grp_curves(grp.curve.file)
  }
    
  # Rank raster file is named *.rank.*
  rank.raster.file <- get_file(root, "\\.rank\\.")
  if (!is.na(rank.raster.file)) {
    .Object@rank <- raster(rank.raster.file)  
  }
  
  wrscr.raster.file <- get_file(root, "\\.wrscr\\.")
  if (!is.na(wrscr.raster.file)) {
    .Object@wrscr <- raster(wrscr.raster.file)  
  }
  
  prop.raster.file <- get_file(root, "\\.prop\\.")
  if (!is.na(prop.raster.file)) {
    .Object@prop <- raster(prop.raster.file)  
  }
  
  f <- validObject(.Object)
  
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
  
  f <- validObject(.Object)
  
  # NOTE: dat-file and spp-file existence has already been checked by zvariant
  # initializer checker function
  
  # dat-file content ###########################################################
  .Object@dat.data <- read_ini(call.params$dat.file)
  
  # spp-file content ###########################################################
  spp.data <- read_spp(call.params$spp.file)

  # Extract spp feature "names" from the raster file path
  spp.data$name <- basename(tools::file_path_sans_ext(spp.data$filepath))
  
  .Object@spp.data <- spp.data
  
  # output dir #################################################################
  
  .Object@output.dir <- call.params$output.folder
  
  # groups content #############################################################
  
  # First we need to define whether groups are 1) used, and 2) available. 
  settings <- .Object@dat.data$Settings
  if ("use_groups" %in% names(settings)) {
    if (settings$use_groups == 1) {
      if ("groups_file" %in% names(settings)) {
        groups.file <- check_path(settings$groups_file, dirname(bat.file),
                                  require.file=TRUE)
        .Object@groups <- read_groups(groups.file)
      }
    }
  }
  
  # results ####################################################################
  
  output.folder <- .Object@call.params$output.folder
  .Object@results <- new("Zresults", root=.Object@call.params$output.folder)
  
  .Object
})
