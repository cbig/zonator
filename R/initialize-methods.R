# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomaki <joona.lehtomaki@gmai.com>. All rights
# reserved.

# This program is open source software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# Zproject ----------------------------------------------------------------

#' Create an instance of the Zproject class using new/initialize.
#'
#' @param root Character string path to the root of the project (must exist).
#' @param debug logical indicating whether debug logging is used
#'
#' @seealso \code{\link{initialize}}
#'
#' @keywords internal
#'
#'
#' @rdname initialize-methods
#' @author Joona Lehtomaki <joona.lehtomaki@@gmail.com>
#'
setMethod("initialize", "Zproject", function(.Object, root, debug=FALSE) {

  if (!file.exists(root)) {
    stop(paste0("Root folder ", root, " not found"))
  } else {
    .Object@root <- root
  }

  assign("debug", debug, envir = .options)

  variants <- list()

  # List all the bat-files
  bat.files <- list.files(root, ".bat$", full.names = TRUE, recursive = TRUE)

  for (bat.file in bat.files) {
    variant <- new("Zvariant", bat.file = bat.file)
    # FIXME: Suppress warning from implicit list embeddinf of S4 objects,
    # which was deprecated in R 3.3.0.
    # https://stat.ethz.ch/pipermail/r-devel/2016-May/072730.html
    suppressWarnings(variants[variant@name] <- variant)
  }

  .Object@variants <- variants

  return(.Object)
})

# Zresults ----------------------------------------------------------------

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
#' @import raster
#' @import rgdal
#' @importFrom methods validObject
#'
#' @rdname initialize-methods
#' @author Joona Lehtomaki <joona.lehtomaki@@gmail.com>
#'
setMethod("initialize", "Zresults", function(.Object, root) {

  # Find various result files from the root path which must exist
  if (!file.exists(root)) {
    warning("Results root path ", root, " does not exist")
    return(.Object)
  }

  .Object@root = root

  # Helper function
  get_file <- function(output.folder, x) {

    target <- list.files(output.folder, pattern = x, full.names = TRUE)
    if (length(target) == 0) {
      return(NA)
    } else if (length(target) == 1) {
      return(target)
    } else {
      warning(paste("More matches than 1 found for", x, "using only the first"))
      return(target[1])
    }
  }

  # Extract the 'last modified' information from run info file. Note that
  # file.info$mtime won't work on Linux, hence parse the data from Zonation
  # run info.
  run.info.file <- get_file(root, "\\.run_info\\.txt")
  if (!is.na(run.info.file)) {

    .Object@run.info <- run.info.file
    content <- readLines(run.info.file)

    # We're counting on hard coded values here
    match_content <- content[grepl("^Finished at", content)]
    if (length(match_content) == 1) {
      # Oh brother...
      date_string <- regmatches(match_content, gregexpr("(?<=\\().*?(?=\\))",
                                                        match_content,
                                                        perl = T))[[1]]
      .Object@modified <- as.POSIXct(date_string)
      if (.options$debug) {
        message("Found run info file modified on: ", .Object@modified)
      }
    }


  } else {
    .Object@modified <- Sys.time()
    if (.options$debug) {
      message("Did not find run info file")
    }
  }

  # Features info file is named *.features_info.txt
  features.info.file <- get_file(root, "\\.features_info\\.txt$")
  if (!is.na(features.info.file)) {
    if (.options$debug) {
      message("Reading in features info file ", features.info.file)
    }
    .Object@features.info <- read_features_info(features.info.file)
  }

  # Curves file is named *.curves.txt. NOTE: if the file does not exist,
  # returns NA.
  curve.file <- get_file(root, "\\.curves\\.txt")
  if (!is.na(curve.file)) {
    if (.options$debug) {
      message("Reading in curve file ", curve.file)
    }
    .Object@curves <- read_curves(curve.file)
  }

  # Group curves file is named *.grp_curves.txt. NOTE: if the file does not
  # exist, returns NA.
  grp.curve.file <- get_file(root, "\\.grp_curves\\.txt")
  if (!is.na(grp.curve.file)) {
    if (.options$debug) {
      message("Reading in groups curve file ", grp.curve.file)
    }
    #
    .Object@grp.curves <- read_grp_curves(grp.curve.file)
  }

  # Rank raster file is named *.rank.*
  rank.raster.file <- get_file(root, "\\.rank\\..+[(img)|(tif)|(asc)]$")
  if (!is.na(rank.raster.file)) {
    if (.options$debug) {
      message("Reading in rank raster file ", rank.raster.file)
    }
    .Object@rank <- raster::raster(rank.raster.file)
  }

  wrscr.raster.file <- get_file(root, "\\.wrscr\\.")
  if (!is.na(wrscr.raster.file)) {
    if (.options$debug) {
      message("Reading in wrscr raster file ", wrscr.raster.file)
    }
    .Object@wrscr <- raster(wrscr.raster.file)
  }

  prop.raster.file <- get_file(root, "\\.prop\\.")
  if (!is.na(prop.raster.file)) {
    if (.options$debug) {
      message("Reading in prop raster file ", prop.raster.file)
    }
    .Object@prop <- raster(prop.raster.file)
  }

  # PPA (post-processing analysis) results may be present as well
  # Process LSM results if present
  # http://cbig.it.helsinki.fi/development/projects/zonation/wiki/LSM_with_pre-defined_units
  ppa.lsm.file <- get_file(root, ".*nwout\\.1.*")
  if (!is.na(ppa.lsm.file)) {
    if (.options$debug) {
      message("Reading in ppa lsm file ", ppa.lsm.file)
    }
    # read_ppa_lsm return a list. Merge data items 1 and 3, don't use 2
    ppa.lsm.data <- read_ppa_lsm(ppa.lsm.file)
    ppa.lsm.data <- merge(ppa.lsm.data[[1]], ppa.lsm.data[[3]], by.x = "Unit",
                      by.y = "Unit_number")
    # Remove Area_cells column as it's redundant
    .Object@ppa.lsm <- ppa.lsm.data[, !(names(ppa.lsm.data) %in% c("Area_cells"))]
  }

  invisible(validObject(.Object))

  return(.Object)
})

# Zvariant ----------------------------------------------------------------

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
#' @importFrom methods validObject
#'
#' @rdname initialize-methods
#' @author Joona Lehtomaki <joona.lehtomaki@@gmail.com>
#'
setMethod("initialize", "Zvariant", function(.Object, name=NULL, bat.file) {

  if (!file.exists(bat.file)) {
    stop(paste0("Variant .bat-file does not exist: ", bat.file))
  }

  # Set the name

  if (is.null(name)) {
    # If no name is provided, use the name of the bat-file (without the
    # extension)
    .Object@name <- strsplit(basename(bat.file), "\\.")[[1]][1]
  } else {
    .Object@name <- name
  }

  .Object@bat.file <- bat.file
  # Read the content of the bat file
  if (.options$debug) {
    message("Reading in bat file ", bat.file)
  }
  call.params <- read_bat(bat.file)
  .Object@call.params <- call.params

  invisible(validObject(.Object))

  # NOTE: dat-file and spp-file existence has already been checked by zvariant
  # initializer checker function

  # dat-file content
  if (.options$debug) {
    message("Reading in dat file ", call.params$dat.file)
  }
  .Object@dat.data <- read_dat(call.params$dat.file)

  # spp-file content
  if (.options$debug) {
    message("Reading in spp file ", call.params$spp.file)
  }
  spp.data <- read_spp(call.params$spp.file)

  # Extract spp feature "names" from the raster file path
  spp.data$name <- basename(zonator::file_path_sans_ext(spp.data$filepath))

  .Object@spp.data <- spp.data

  # output dir

  .Object@output.dir <- call.params$output.folder

  # groups content

  # First we need to define whether groups are 1) used, and 2) available.
  use_groups <- get_dat_param(.Object, "use groups", warn_missing = FALSE)
  if (!is.na(use_groups) & use_groups == 1) {
    groups_file <- get_dat_param(.Object, "groups file", warn_missing = FALSE)
    groups_file <- check_path(groups_file, dirname(bat.file),
                              require.file = TRUE)
    if (.options$debug) {
      message("Reading in groups file ", groups_file)
    }
    .Object@groups <- read_groups(groups_file)
    # Initialize generic group names "group1", "group2" etc
    group.ids <- unique(.Object@groups$output.group)
    group.names <- paste0("group", group.ids)
    names(group.names) <- group.ids
    groupnames(.Object) <- group.names
  }

  # Condition layers

  # Again, check that condition(s) are 1) used and 2) available.
  use_condition_layer <- get_dat_param(.Object, "use condition layer",
                                       warn_missing = FALSE)
  if (!is.na(use_condition_layer) & use_condition_layer == 1) {
    condition_file <- get_dat_param(.Object, "condition file",
                                    warn_missing = FALSE)
    condition_file <- check_path(condition_file, dirname(bat.file),
                                 require.file = TRUE)
    if (.options$debug) {
      message("Reading in condition file ", condition_file)
    }
    .Object@condition.layers <- read.table(condition_file,
                                          col.names = c("group", "raster"))
  }

  # results
  .Object@results <- new("Zresults", root = .Object@call.params$output.folder)

  # Make sure that correct groupIDs are used also to name group curves in
  # results
  if (use_groups == "1" & has_results(.Object)$grp.curves) {
    .Object@results@grp.curves <- regroup_curves(curves(.Object),
                                                 sppweights(.Object),
                                                 groups(.Object))
  }

  featurenames(.Object) <- spp.data$name

  # Use an internal variable to track whether results are procuded by the
  # current spp data and settings.
  .Object@results_dirty <- FALSE

  return(.Object)
})
