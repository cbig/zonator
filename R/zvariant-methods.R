# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomaki <joona.lehtomaki@gmai.com>. All rights
# reserved.

# This program is open source software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# copy_zvariant -----------------------------------------------------------

#' @rdname copy_zvariant-methods
#' @aliases copy_zvariant
#'
setMethod("copy_zvariant", signature("Zvariant", "character", "character"),
          function(x, name, dir) {
  # Copying is effectively replacing the old variant name and path with the
  # new ones in ALL locations affected (also within the actual configuration
  # files). Get all the components that need replacing.
  old_name <- x@name
  old_bat_file <- x@bat.file
  old_root <- dirname(old_bat_file)
  old_spp_file <- x@call.params$spp.file
  old_dat_file <- x@call.params$dat.file
  old_spp_data <- x@spp.data
  old_dat_data <- x@dat.data
  old_call_params <- x@call.params
  old_output_dir <- x@output.dir

  new_name <- name
  new_root <- dir

  if (!dir.exists(new_root)) {
    message("Creating new root directory at ", new_root)
    dir.create(new_root, recursive = TRUE)
  }

  # Full path includes also dir
  new_full_path <- file.path(new_root, new_name)

  # Saving a Zvariant does create the necessary directories. Do this manually
  dir.create(new_full_path, recursive = TRUE)

  # Make a copy of the Zvariant object
  new_x <- x

  # Make all replacements
  # Name
  new_x@name <- new_name
  # bat-file
  new_bat_file <- paste0(new_full_path, ".bat")
  new_x@bat.file <- new_bat_file
  # spp-file
  new_spp_file <- gsub(old_name, name, old_spp_file)
  new_spp_file <- gsub(old_root, dir, old_spp_file)
  new_x@call.params$spp.file <- new_spp_file
  # dat-file
  new_dat_file <- gsub(old_name, name, old_dat_file)
  new_dat_file <- gsub(old_root, dir, old_dat_file)
  new_x@call.params$dat.file <- new_dat_file
  # spp data
  new_spp_data <- old_spp_data
  new_spp_data$filepath <- sapply(new_spp_data$filepath, file_path_relative_to,
                                  old_bat_file, new_bat_file, USE.NAMES = FALSE)
  new_x@spp.data <- new_spp_data
  # dat data
  # FIXME: Current implementation does not account for all the possible
  # parameters that might need replacing in the dat file. Needs to be
  # expanded.
  new_dat_data <- old_dat_data
  new_x@dat.data[['Settings']] <- lapply(new_dat_data[['Settings']],
                                         function(x) gsub(old_name, new_name, x))

  # call parameters
  new_call_params <- old_call_params
  new_call_params <- lapply(new_call_params,
                            function(x) {
                              x <- gsub(old_root, new_root, x)
                              x <- gsub(old_name, new_name, x)
                              return(x)
                            })
  new_x@call.params <- new_call_params

  # Output.dir
  new_output_dir <- gsub(old_root, new_root, old_output_dir)
  new_output_dir <- gsub(old_name, new_name, new_output_dir)
  new_x@output.dir <- new_output_dir
  dir.create(new_output_dir, recursive = TRUE)

  # Reset results as these are not copied anyways
  new_x@results <- new("Zresults", root = new_x@call.params$output.folder)

  # Save everything
  save_zvariant(new_x, overwrite = TRUE)

  return(new_x)

})

# cost --------------------------------------------------------------------

#' @rdname cost-methods
#' @aliases cost,Zresults-method
#'
setMethod("cost", signature("Zvariant"), function(x) {
  if (has_results(x)$curves) {
    cost.data <- cost(results(x))
  } else {
    cost.data <- NA
    warning("No results (curves) data available")
  }
  return(cost.data)
})

# curves ------------------------------------------------------------------

#' @rdname curves-methods
#' @aliases curves,Zresults-method
#'
setMethod("curves", c("Zvariant"), function(x, cols=NULL, groups=FALSE,
                                            lost.lower=0.0, lost.upper=1.0) {
 return(curves(results(x), cols, groups, lost.lower, lost.upper))
})

# featurenames ------------------------------------------------------------

#' @rdname featurenames-methods
#'
setMethod("featurenames", signature("Zvariant"), function(x) {

  if (all(is.na(x@spp.data))) {
    stop("No spp data found")
  }
  if (!"name" %in% names(x@spp.data)) {
    stop("spp data doesn't have 'name' column defined")
  }

  return(x@spp.data$name)
})

# featurenames<- ----------------------------------------------------------

#' @name featurenames<-
#' @rdname featurenames-methods
#' @aliases featurenames<-,Zvariant,character-method
#'
setReplaceMethod("featurenames", c("Zvariant", "character"), function(x, value) {

  # Check names
  value <- check_names(value)
  # Control for length, no cycling allowed
  if (length(value) != nrow(x@spp.data)) {
    stop(paste0("Character vector length (", length(value), " and object spp ",
                "data length (", nrow(x@spp.data), " should be the same"))
  }
  x@spp.data$name <- value
  # Also deal with the results data if available
  if (any(dim(x@results@curves) != c(0, 0))) {
    featurenames(x@results@curves) <- value
  }
  return(x)
})

# features_info -----------------------------------------------------------

#' @rdname features_info-methods
#' @aliases features_info,Zresults-method
#'
setMethod("features_info", c("Zvariant"), function(x) {
  if (has_results(x)$features.info) {
    return(results(x)@features.info)
  } else {
    warning("Features info data requested but not present in ", outdir(x))
  }
})

# get_dat_param -----------------------------------------------------------

#' @rdname get_dat_param-methods
#'
setMethod("get_dat_param", signature("Zvariant"),
          function(x, parameter, warn_missing=TRUE) {
  # Only canocical parameters are accepted
  if (!parameter %in% zparameters(just_names = TRUE)) {
    stop("Requested parameter not valid Zonation parameter.")
  }

  # Check the current parameters. If requested parameter is valid Zonation
  # parameter but not currently set, return NA.
  current_params <- leaf_tags(x@dat.data, omit_sections = TRUE)

  if (!parameter %in% names(current_params)) {
    if (warn_missing) {
      warning("Requested parameter valid Zonation parameter, but not set.")
    }
    return(NA)
  }

  # Return the correct parameter value
  return(as.vector(current_params[parameter]))
})

# groups ------------------------------------------------------------------

#' @rdname groups-methods
#'
setMethod("groups", "Zvariant", function(x) {
  if (any(dim(x@groups) != c(0, 0))) {
    return(x@groups$output.group)
  } else {
    return(NA)
  }
})

# groups<- ----------------------------------------------------------------

#' @name groups<-
#' @rdname groups-methods
#' @aliases groups<-,Zvariant,numeric-method
#'
setReplaceMethod("groups", c("Zvariant", "numeric"), function(x, value) {

  # Check that the number of provided group codes matches with the number of
  # features
  nfeats <- nfeatures(x)
  nreplacement <- length(value)

  if (nfeats > nreplacement) {
    stop("Too few replacement group IDs (", nreplacement, ") for variant ",
         "features (", nfeats, ")")
  } else if (nfeats < nreplacement) {
    stop("Too many replacement group IDs (", nreplacement, ") for variant ",
         "features (", nfeats, ")")
  }

  # If nothing has been assigned, assign values. If nothing changes, do nothing.
  if (is.null(x@groups$output.group) | !all(x@groups$output.group == value)) {
    # In case no groups dataframe has been populated, do that
    if (nrow(x@groups) == 0L) {
      # FIXME: not a good idea to define names(x@groups) in several places...
      x@groups <- data.frame(output.group = value,
                             condition.group = -1,
                             retention.group = -1,
                             retention.mode = 1,
                             local.edge.correct.group = -1,
                             name = "")
    } else {
      x@groups$output.group <- value
    }
    # Update results curves data if it exists
    #browser()
    if (has_results(x)[["curves"]]) {
      x@results@grp.curves <- regroup_curves(curves(x), sppweights(x), value)
    }
    # Change group names back to generic "group1", "group2" etc.
    group.ids <- unique(value)
    group.names <- paste0("group", group.ids)
    names(group.names) <- group.ids
    groupnames(x) <- group.names
  }

  return(x)
})

# groupnames --------------------------------------------------------------

#' @rdname groupnames-methods
#' @export
#'
setMethod("groupnames", "Zvariant", function(x) {

  if (any(dim(x@groups) == c(0, 0))) {
    return(NA)
  }

  # Get all the groups data
  groups.data <- x@groups
  # Get unique codes
  groups.codes <- unique(groups.data$output.group)
  groups.names <- sapply(groups.codes, function(y) {groups.data[which(groups.data$output.group == y),]$name[1]})
  return(groups.names)
})

# groupnames<- ------------------------------------------------------------

#' @name groupnames<-
#' @rdname groupnames-methods
#' @aliases groupnames<-,Zvariant,character-method
#'
setReplaceMethod("groupnames", c("Zvariant", "character"), function(x, value) {
  if (nrow(x@groups) == 0) {
    stop("Variant has no groups to name")
  }
  # NAs are not allowed as group names
  if (any(is.na(names(value)))) {
    stop("Group names cannot include NAs")
  }

  # Actual coded values are vector names. Assume numeric and try to coerce.
  keys <- as.numeric(names(value))
  group.codes <- groups(x)
  unique.group.codes <- unique(group.codes)
  # Check that all group codes are actually found in the keys
  if (!all(unique.group.codes %in% keys)) {
    stop(paste("Group code(s)", paste(unique.group.codes[!unique.group.codes %in% keys],
                                      collapse = ", "),
                               "not found in keys:",
               paste(keys, collapse = ", ")))
  }
  # Subset only values that are used (there can be more)
  value <- value[which(names(value) %in% unique.group.codes)]

  # Get the actual character vector indexes based on the names
  inds <- sapply(group.codes, function(y) {which(keys == y)})
  # Index the value vector
  x@groups$name <- value[inds]

  if (has_results(x)$grp.curves) {
    results.grp.names <- names(x@results@grp.curves)[3:length(x@results@grp.curves)]

    # Each group has 5 columns
    ngroups <- length(results.grp.names) / 5

    if (length(value) != ngroups) {
      stop(paste0("Character vector length (", length(value), ") and object ",
                  "results group curves header length (", ngroups,
                  ") should be the same"))
    }
    for (i in 1:ngroups) {
      group.id <- names(value[i])
      group.name  <- value[[i]]
      results.grp.names <- gsub(paste0("group", group.id, "$"), group.name,
                                results.grp.names)
    }
    new.grp.names <- c(names(x@results@grp.curves)[1:2], results.grp.names)
    names(x@results@grp.curves) <- new.grp.names
  }
  return(x)
})

# has_results -------------------------------------------------------------

#' @rdname has_results-methods
#'
setMethod("has_results", "Zvariant", function(x) {
  return(has_results(x@results))
})

#' @rdname nfeatures-methods
#' @aliases nfeatures,Zvariant-method
#'
setMethod("nfeatures", "Zvariant", function(x) {
  return(nrow(x@spp.data))
})

# outdir ------------------------------------------------------------------

#' @rdname outdir-methods
#'
setMethod("outdir", c("Zvariant"), function(x) {
  return(x@output.dir)
})

# print -------------------------------------------------------------------

#' Print Zvariant information.
#'
#' Generic printing function
#'
#' @param x \code{ZVariant} object.
#'
#' @rdname print-methods
#'
#' @export
#'
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#'
setMethod('print' , c("Zvariant"), function(x) {
  .printZvariant(x)
})

# rank_raster -------------------------------------------------------------

#' @rdname rank_raster-methods
#'
setMethod("rank_raster", c("Zvariant"), function(x) {
  if (has_results(x)$rank) {
    return(rank_raster(results(x)))
  } else {
    warning("Rank raster requested but not present in ", outdir(x))
  }
})

# results -----------------------------------------------------------------

#' @rdname results-methods
#'
setMethod("results", c("Zvariant"), function(x) {
  res <-  unlist(has_results(x))
  # Return results whenever there is at least 1 result item available
  if (any(res)) {
    return(x@results)
  } else {
    warning("Variant doesn't have results")
    return(NA)
  }
})

# save_zvariant -----------------------------------------------------------

#' @rdname save_zvariant-methods
#'
setMethod("save_zvariant", signature("Zvariant", "ANY", "ANY", "ANY"),
          function(x, dir="", overwrite=FALSE, debug_msg=FALSE) {

  if (dir == "") {
    # Only use x@bat.file; x@name is derived from that and there's no
    # syncing/checking between the two.
    parent_dir <- dirname(x@bat.file)
  } else {
    if (dir.exists(dir)) {
      parent_dir <- dir
      bat_file <- file.path(parent_dir, basename(x@bat.file))
    } else {
      stop("dir doesn't exist.")
    }
  }

  bat_file <- x@bat.file
  spp_file <- x@call.params$spp.file
  dat_file <- x@call.params$dat.file
  variant_name <- gsub("\\.bat", "", basename(bat_file))
  variant_dir <- file.path(parent_dir, variant_name)

  # Check for overwriting. NOTE: there could more optional files that are
  # overwritten (e.g. groups), but checking the mandatory files should be
  # enough.
  if (overwrite) {
    # Delete old files
    unlink(variant_dir, recursive = TRUE, force = TRUE)
    if (debug_msg) message("Deleted existing variant dir ", variant_dir)
  } else {
    if (any(file.exists(c(bat_file, spp_file, dat_file)))) {
      stop("Cannot save: at least some of the variant files exist and overwrite is off.")
    }
  }

  # Create variant folder
  dir.create(variant_dir)
  if (debug_msg) message("Created variant directory ", variant_dir)

  # dat-file
  dat_to <- file.path(variant_dir, paste0(variant_name, ".dat"))
  write_dat(x@dat.data, dat_to, overwrite = TRUE)
  if (debug_msg) message("Wrote dat file ", dat_to)

  # Write other files listed in dat-file.
  #
  # Groups
  if (!is.na(get_dat_param(x, "use groups", warn_missing = FALSE))) {
    groups_file <- get_dat_param(x, "groups file", warn_missing = FALSE)
    if (is.na(groups_file)) {
      warning("'use groups' is 1, but no groups file path is defined.")
    } else {
      groups_to <- file.path(parent_dir, groups_file)
      groups_data <- x@groups
      # Remove "name" field
      groups_data <- groups_data[,!(names(groups_data) %in% c("name"))]
      write.table(groups_data, file = groups_to, row.names = FALSE, sep = "\t",
                  col.names = FALSE, quote = FALSE)
      if (debug_msg) message("Wrote groups file ", groups_to)
    }
  }

  # spp-file
  # Remove "name" and "group"
  spp_data <- sppdata(x)
  spp_data <- spp_data[,!(names(spp_data) %in% c("name", "group"))]
  spp_to <- file.path(variant_dir, paste0(variant_name, ".spp"))
  # Format weights and alpha to get a better layout. First, find out how many
  # decimal places are needed.
  weight_formatter <- paste0("%.", max(decimalplaces(spp_data$weight)), "f")
  alpha_formatter <- paste0("%.", max(decimalplaces(spp_data$alpha)), "f")

  spp_data$weight <- sprintf(weight_formatter, spp_data$weight)
  spp_data$alpha <- sprintf(alpha_formatter, spp_data$alpha)
  write.table(spp_data, file = spp_to, row.names = FALSE, col.names = FALSE,
              quote = FALSE)
  if (debug_msg) message("Wrote spp file ", spp_to)

  # Create to output folder
  output_dir <- file.path(variant_dir, paste0(variant_name, "_out"))
  dir.create(output_dir, recursive = TRUE)
  if (debug_msg) message("Created output directory ", output_dir)

  # Create a bat file content
  dat_relative <- gsub(paste0(parent_dir, .Platform$file.sep), "", dat_to)
  spp_relative <- gsub(paste0(parent_dir, .Platform$file.sep), "", spp_to)
  output_dir_relative <- gsub(paste0(parent_dir, .Platform$file.sep), "",
                              output_dir)

  cmd_sequence <- x@call.params
  cmd_sequence$dat.file <- dat_relative
  cmd_sequence$spp.file <- spp_relative
  cmd_sequence$output.folder <- file.path(output_dir_relative,
                                          paste0(variant_name, ".txt"))

  # Additional command switches for Zonation
  cmd_sequence <- c("dos_call"="call", cmd_sequence)
  cmd_sequence$grid.type <- "--grid-output-formats=compressed-tif"
  cmd_sequence$image.type <- "--image-output-formats=png"

  # Flatten the list
  cmd_sequence <- unlist(cmd_sequence)

  # Write bat-file
  bat_to <- file.path(parent_dir, paste0(variant_name, ".bat"))
  cat(paste0(paste(cmd_sequence, collapse = " "), "\n"), file = bat_to)
  if (debug_msg) message("Wrote bat file ", bat_to)

  message("Wrote variant '", variant_name, "' into ", variant_dir)
})

# set_dat_param -----------------------------------------------------------

#' @rdname set_dat_param-methods
#'
setMethod("set_dat_param", signature("Zvariant"), function(x, parameter, value) {
  # Only canocical parameters are accepted
  if (!parameter %in% zparameters(just_names = TRUE)) {
    stop("Parameter not a valid Zonation parameter name.")
  }
  # Zonation dat-file (and thus x@dat.data) can only have 2 levels in the
  # nested list: section and parameter. Hence we can safely loop through the
  # list.
  for (section_name in names(x@dat.data)) {
    for (parameter_name in names(x@dat.data[[section_name]])) {
      # Use zparameters() to match the section
      x@dat.data[[zparameters()[[parameter]]]][[parameter]] <- value
    }
  }
  x@results_dirty <- TRUE
  return(x)
})

# show --------------------------------------------------------------------

#' Print Zvariant information.
#'
#' Generic printing function
#'
#' @param object \code{ZVariant} object.
#'
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#'
setMethod("show" , c("Zvariant"), function(object) {
  .printZvariant(object)
})

# sppdata -----------------------------------------------------------------

#' @rdname sppdata-methods
#'
setMethod("sppdata", c("Zvariant"), function(x, group.names=FALSE) {
  spp.data <- x@spp.data
  if (nrow(x@groups) > 0) {
    spp.names <- names(spp.data)
    if (group.names == TRUE && "name" %in% names(x@groups)) {
      spp.data <- cbind(spp.data, x@groups$name)
      names(spp.data) <- c(spp.names, "group.name")
    } else {
      spp.data <- cbind(spp.data, x@groups$output.group)
      names(spp.data) <- c(spp.names, "group")
    }
  }
  return(spp.data)
})

# sppdata<- ---------------------------------------------------------------

#' @name sppdata<-
#' @rdname sppdata-methods
#' @aliases sppdata<-,Zvariant,data.frame-method
#'
setReplaceMethod("sppdata", c("Zvariant", "data.frame"), function(x, value) {
  # Check the number and names of columns
  if (ncol(value) != 7) {
    stop("Incorrect number of columns in assigning spp data.")
  }
  if (!any(names(value) %in% names(x@spp.data))) {
    stop("Incorrect column names in assigning spp data. Permitted names are: ",
         paste(names(x@spp.data), collapse = ", "))
  }
  x@spp.data <- value
  # spp data changed, results are no longer in sync if present
  if (any(unlist(has_results(x)))) {
    x@results_dirty <- TRUE
    warning("sppdata has changed, results may not be in sync with the current state of the Zvariant object.")
  }

  # If groups are used, default the groups information
  if (nrow(x@groups) > 0) {
    nrows <- nrow(x@spp.data)
    x@groups <- data.frame(output.group = rep(1, nrows),
                           condition.group = rep(-1, nrows),
                           retention.group = rep(-1, nrows),
                           retention.mode = rep(1, nrows),
                           local.edge.correct.group = rep(-1, nrows),
                           name = rep("group1", nrows))
  }

  return(x)
})

# sppweights --------------------------------------------------------------

#' @rdname sppweights-methods
#'
setMethod("sppweights", c("Zvariant"), function(x) {
  return(x@spp.data$weight)
})

# sppweights<- ------------------------------------------------------------

#' @name sppweights<-
#' @rdname sppweights-methods
#' @aliases sppweights<-,Zvariant,numeric-method
#'
setReplaceMethod("sppweights", c("Zvariant", "numeric"), function(x, value) {
  # Check that the number of weight items is correct
  if (length(value) != nfeatures(x)) {
    stop("The number of weights (", length(value),
         ") and the number of features (", nfeatures(x), ") is different.")
  }
  # Check that the type of weight items is correct
  value <- suppressWarnings(as.numeric(value))
  if (any(is.na(value))) {
    stop("Weight vector elements must be coercible to unmeric.")
  }
  x@spp.data$weight <- value
  return(x)
})


# .printZvariant ----------------------------------------------------------

.printZvariant <- function(x, ...) {

  cat('name       :', x@name, '\n')
  cat('bat-file   :', x@bat.file, '\n')
  # Show max first 5 biodiversity feature names
  spp_names <- featurenames(x)
  if (length(spp_names) > 5) {
    spp_names <- paste(c(spp_names[1:5], "..."), collapse = ", ")
  } else {
    spp_names <- paste(c(spp_names), collapse = ", ")
  }
  cat('features   :', nrow(x@spp.data), paste0("[", spp_names, "]"), '\n')
  # Show max first 5 group names
  grp_names <- groupnames(x)
  if (length(grp_names) > 5) {
    grp_names <- paste(c(grp_names[1:5], "..."), collapse = ", ")
  } else {
    grp_names <- paste(c(grp_names), collapse = ", ")
  }
  cat('groups     :', length(unique(groupnames(x))),
      paste0("[", grp_names, "]"), '\n')
  if (any(unlist(has_results(x)))) {
    res_string <- paste0("yes (created: ", as.character(x@results@modified),
                         ")")
  } else {
    res_string <- "no"
  }
  cat("has results:", res_string, '\n')
  # Run configuration parameters
  parameter_string <- c("parameters :")
  for (section in names(x@dat.data)) {
    parameter_string <- c(parameter_string,
                          paste0(" [", section, "]"))
    for (parameter in names(x@dat.data[[section]])) {
      parameter_string <- c(parameter_string,
                            paste0("  ", parameter, ": ",
                                   x@dat.data[[section]][[parameter]]))
    }
  }
  cat(paste0(parameter_string, collapse = "\n"))
}
