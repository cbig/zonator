# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomaki <joona.lehtomaki@gmai.com>. All rights
# reserved.

# This program is open source software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' A function check feature/group names.
#'
#' Checks a vector of names only contains unique items and if they're not,
#' unique names will be created. Also, the items must be
#' suitable for columns names. Function is strict so that if the vector is not
#' valid or it cannot be coerced to be one an error is induced.
#'
#' @param x Charcter or numeric vector.
#'
#' @return Valid vector of the original size.
#'
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export
#'
check_names <- function(x) {
  # Check for type
  if (!any(is.character(x), is.numeric(x))) {
    stop("Names vector must be either character of numeric")
  }
  # Check for only unique items
  if (length(unique(x)) != length(x)) {
    warning("All feature/group names are not unique, creating unique names")
    x <- make.names(x, unique=TRUE)
  }
  # Check for empty names
  if (any(x == "") || any(nchar(x) == 0)) {
    stop("No item in names vector can be empty")
  }
  # Get rid of white space if present
  if (any(grepl("\\s", x))) {
    warning("Name items contain whitespaces, replacing with '.'")
    x <- gsub("\\s", "\\.", x)
  }
  return(as.character(x))
}

#' A function to deal with potentially relative paths.
#'
#' Checks if a path can be resolved (i.e. whether it exists). An additional
#' parameter \code{parent.path} can be provided, in which case \code{x} is
#' appended to it and the concatenated path is checked for existence. If the
#' path cannot be resolved, raise an error.
#'
#' @param x Character string path.
#' @param parent.path Character string root path.
#' @param require.file Logical indicating if a file is required for return or
#' if an existing parent folder is enough
#'
#' @return A cleaned character string
#'
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export
#'
check_path <- function(x, parent.path=NULL, require.file=FALSE) {
  # Expand path
  x <- path.expand(x)
  # Replace "\\" with "/"
  x <- gsub("\\\\", "/", x)

  # Deal with potentially relative paths in x. Note that if there is no
  # parent.path relative paths do not make any difference.
  if (grepl("\\.{2}/", x) && !is.null(parent.path)) {
    match <- gregexpr("\\.{2}/", x)[[1]]
    # How many '../' token are there in x?
    dot.tokens <- length(attr(match, "match.length"))
    # Get rid of the tokens
    x <- gsub("\\.{2}/", "", x)
    # Break the parent path to elements
    dir.elements <- unlist(strsplit(parent.path, .Platform$file.sep))
    parent.path <- paste(dir.elements[1:(length(dir.elements) - dot.tokens)],
                         collapse = .Platform$file.sep)
  }

  # Is x valid file path on its own?
  if (file.exists(x)) {
    return(x)
  } else if (!is.null(parent.path)) {
    path <- file.path(parent.path, x)
    # Is x a valid file path when combined with the parent.path?
    if (file.exists(path)) {
      return(path)
    } else {
      # Is the parent path at least valid?
      if (file.exists(parent.path) && !require.file) {
        return(parent.path)
      } else {
        stop("Path ", file.path(parent.path, x), " cannot be resolved.")
      }
    }
  } else {
    stop("Path ", x, parent.path, " cannot be resolved.")
  }
}

#' Clean leading and trailing whitespaces from a given string. Additionally,
#' all occurrences of multiple whitespaces are replaced with a single
#' whitespace.
#'
#' @param x Character string.
#'
#' @return An absolute path to a file of NULL if the path does not exist.
#'
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export
#'
clean_str <- function(x) {

  x <- gsub("\\s+", " ", x)

  # returns string w/o leading or trailing whitespace
  x <- gsub("^\\s+|\\s+$", "", x)
  return(x)
}

#' Find out the number of decimal places in a number.
#'
#' Original implementation from https://stackoverflow.com/questions/5173692/how-to-return-number-of-decimal-places-in-r
#'
#' @note R usually restricts the number of decimal to 9 in printing etc. Unless
#' \code{true_number = TRUE}, return 9 and give a warning.
#'
#' @param x Float or double numeric number.
#' @param true_number Logical setting whether the true number (see notes) of
#'   decimal places.
#'
#' @return Integer number of decimal places. Maximum
#'
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export
#'
decimalplaces <-
  Vectorize(function(x, true_number = FALSE) {
    if ((x %% 1) != 0) {
      decimals <- nchar(strsplit(sub('0+$', '', as.character(x)), ".",
                                 fixed = TRUE)[[1]][[2]])
    } else {
      return(0)
    }
    if (decimals > 9 & true_number == FALSE)  {
      decimals <- 9
      warning("Number of decimal places more than 9 but true_number set to FALSE")
    }
    return(decimals)
  },
  c("x"), USE.NAMES = TRUE)

#' Transform an absolute path to relative path in relation to given location
#'
#' @note Both \code{path} and \code{relative_to} must be in absolute form.
#'
#' @param path Character string path.
#' @param org_relative_to Character string path to which \code{path} is originally
#'                        relative to.
#' @param new_relative_to Character string path to which \code{path} is supposed to
#'                        be relative to.
#'
#' @return Character string relative file path.
#'
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export
#'
file_path_relative_to <- function(path, org_relative_to, new_relative_to) {

  # Check if the path provided is relative. If it is, make it absolute
  if (startsWith(path, "..")) {
    path <- normalizePath(file.path(dirname(org_relative_to), path), mustWork = FALSE)
  }

  # Get path components split by path separators
  path_comps <- unlist(strsplit(dirname(path), split = .Platform$file.sep))
  relative_to_comps <- unlist(strsplit(dirname(new_relative_to), split = .Platform$file.sep))

  # Compare path components
  suppressWarnings(diff <- path_comps == relative_to_comps)
  # Get the file name
  file_path_base <- basename(path)
  # Get path elements equal to the lenght of relative_to_comps: this number is used
  # to generate the correct amount ".." in the path
  rel_path <- paste0(rep("..", sum(!diff[1:length(relative_to_comps)])),
                     collapse = .Platform$file.sep)
  # Construct the actual directory part
  dir_path <- paste0(path_comps[!diff[1:length(path_comps)]],
                     collapse = .Platform$file.sep)
  # Put everything together
  rel_file_path <- file.path(rel_path, dir_path, file_path_base)
  return(rel_file_path)
}

#' Re-implementation of \code{\link{file_path_sans_ext}} in \code{tools}. This
#' version can handle "." just before the file extenstion, unlike the original
#' implementation.
#'
#' @param x Character vector giving file paths.
#' @param compression	Logical: should compression extension '.gz', '.bz2' or
#'   '.xz' be removed first?
#'
#' @return File path without the file extension.
#'
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export
#'
file_path_sans_ext <- function(x, compression = FALSE) {
  if (compression)
    x <- sub("[.](gz|bz2|xz)$", "", x)
  sub("([^.]+.+)\\.[[:alnum:]]+$", "\\1", x)
}

#' Get all Zonation run configuration parameters.
#'
#' This set of parameters is all that is accepted by Zonation.
#'
#' @note Parameters are hard-coded to this package and know nothing
#'   of potential future developments with Zonation.
#'
#' @param just_names Logical indicating if only the parameter names should be
#'   returned.
#'
#' @return Characted vector of paramter names or a list of (parameter = section)
#'   structure.
#'
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export
#'
zparameters <- function(just_names = FALSE) {
  # Only canocical parameters are accepted
  all_parameters_file <- system.file("extdata/template.dat",
                                     package = "zonator")
  if (just_names) {
    accepted_parameters <- leaf_tags(read_dat(all_parameters_file),
                                     omit_sections = TRUE)
    accepted_parameters <- names(accepted_parameters)
  } else {
    accepted_parameters <- leaf_tags(read_dat(all_parameters_file),
                                     omit_sections = FALSE)
    # Split the section.param_name Strings into tuples of (section, param_name)
    accepted_parameters <- strsplit(names(accepted_parameters), "\\.")
    param_list <- list()
    for (item in accepted_parameters) {
      param_list[[item[2]]] <- item[1]
    }
    accepted_parameters <- param_list
  }
  return(accepted_parameters)
}

#' Find all the leaf tags in a potentially nested list. The generic form of a
#' list is tag = value; find all the tags in a list.
#'
#' @param x List to be searched.
#' @param omit_sections Logical indicating if sections should be omitted from
#'   vector names.
#'
#' @return Characted vector of tags.
#'
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export
#' @examples
#' l <- list("a" = 1, "b" = list("c" = 3, "d" = 4), "e" = 5)
#' leaf_tags(l)
#'
leaf_tags <- function(x, omit_sections = FALSE) {
  if (!is.list(x)) {
    stop("Function only accepts lists")
  }
  # Get the tag names, these will include nested tags separated by "."
  tags <- rapply(x, function(x) x[1])

  if (omit_sections) {
    names(tags) <- as.vector(sapply(names(tags),
                                    function(x) tail(unlist(strsplit(x, "\\.")),
                                                     n = 1)))
  }
  return(tags)
}

line_as_numeric <- function(x) {
  return(as.numeric(line_as_string(x)))
}

line_as_string <- function(x) {
  return(unlist(strsplit(x, "\\s+")))
}

#' Map vector to actual column indexes.
#'
#' Compare a vector of column names or indexes against another vector which is
#' known to be true.
#'
#'
#'
#' @param x Character or numeric vector of possible matches.
#' @param y Character or numeric vector of true values.
#'
#' \code{x} and \code{y} must be of the same length.
#'
#' @return A numeric vector of the same length of x and y containing matched
#' column indexes.
#'
#' @keywords zonation results
#' @author Joona Lehtomaki <joona.lehtomaki@@gmail.com>
#'
#' @export
#'

map_indexes <- function(x, y) {
  if (is.character(x)) {
    if (!all(x %in% y)){
      warning(paste("Column names", paste(x[!x %in% y], collapse=", "),
                    "not found in curves header"))
      x <- x[x %in% y]
      if (length(x) == 0) {
        return(NULL)
      }
    }
    inds <- sapply(x, function(xx) {which(xx == y)})
  } else if (is.numeric(x)) {
    inds <- x
    if (any(x < 1)) {
      warning(paste("Column indexes", paste(x[which(x < 1)], collapse=", "),
                    "smaller than 1"))
      inds <- x[which(x >= 1)]
    }
    ncols <- length(y)
    if (any(x > ncols)) {
      warning(paste("Column indexes", paste(x[which(x > ncols)], collapse=", "),
                    "greater than ncol"))
      inds <- x[which(x <= ncols)]
    }
  }
  return(as.numeric(inds))
}

#' Re-calculate group curves data.
#'
#' When results grouping is changed group-specific curves data has to be
#' re-calculated. Normally group curves file is produced by Zonation based on
#' the groupings provided by the user. Same information can almost completely
#' (except for ext-values) be calculated afterwards from the feature-specific
#' curves files.
#'
#' This function calculates the following stats for \code{\link{Zvariant}}
#' object based on a vector of new group IDs:
#'
#' \describe{
#'    \item{\code{min}:}{Minimum value of representation on each iteration among
#'      features within a group.}
#'    \item{\code{mean}:}{Mean value of representation on each iteration among
#'      features within a group.}
#'    \item{\code{max}:}{Maximum value of representation on each iteration among
#'      features within a group.}
#'    \item{\code{w.mean}:}{Weighted (based on feature weight) mean value of
#'      representation on each iteration among features within a group.}
#'  }
#'
#' @note Current implementation does not calculate values for \code{ext2}
#'   (extinction risk). Column \code{ext2} is retained in the returned data
#'   frame for compatibility, but column will be populated with NAs.
#'
#' @param x Data frame of feature specific representation levels.
#' @param weights numeric vector for feature specific weights
#' @param group.ids numeric vector of new group codes. Number of groups must
#'   match with columns in \code{x}.
#'
#' @return \code{ZGroupCurvesDataFrame} with new group statistics.
#'
#' @keywords zonation results
#' @author Joona Lehtomaki <joona.lehtomaki@@gmail.com>
#'
#' @export
#'
regroup_curves  <- function(x, weights, group.ids) {

  # Assume standard Zonation feature-specific curves file structure, which
  # means that feature data starts from column 8.
  features <- x[, 8:ncol(x)]
  # There should be as many weights as is the length of the provided group.ids
  if (length(weights) != length(group.ids)) {
    stop(paste0("Number of weights (", length(weights), ") and group ids (",
                length(group.ids), ") differs"))
  }
  # There should be as many features as is the length of the provided group.ids
  if (ncol(features) != length(group.ids)) {
    stop(paste0("Number of features (", ncol(features), ") and group ids (",
                length(group.ids), ") differs"))
  }
  # Get the unique group ids and loop over all ids. Naming convention for the
  # group stats is:
  # "min.g1" "mean.g1" "max.g1" "w.mean.g1" "ext2.g1"
  ids <- unique(group.ids)
  groups.list <- list()
  for (id in ids) {
    group.names <- paste0(c("min.group", "mean.group", "max.group",
                            "w.mean.group", "ext2.group"), id)
    group.data <- features[, which(group.ids == id)]
    group.weights <- weights[which(group.ids == id)]
    # Calculate row-wise stats, but only if there are more than 1 feature in
    # the group
    if (is.data.frame(group.data)) {
      group.df <- data.frame("min"=apply(group.data, 1, min),
                             "mean"=apply(group.data, 1, mean),
                             "max"=apply(group.data, 1, max),
                             "w.mean"=apply(group.data, 1,
                                            weighted.mean, w=group.weights))
    } else if (is.vector(group.data)) {
      group.df <- data.frame("min"=group.data,
                             "mean"=group.data,
                             "max"=group.data,
                             "w.mean"=group.data)
    }
    group.df$ext2 <- NA
    names(group.df) <- group.names
    groups.list[[as.character(id)]] <- group.df
  }
  # pr_lost and cost should be identical for curves and group curves
  regrouped.x <- cbind(x[, c(1, 2)], do.call("cbind", groups.list))
  # Previous will prefix column names with "id." prefix, get rid of it
  colnames(regrouped.x) <- gsub("^[0-9]+\\.", "", colnames(regrouped.x))
  regrouped.x <- new("ZGroupCurvesDataFrame", regrouped.x,
                    is.group = c(rep(FALSE, 2), rep(TRUE, ncol(regrouped.x) - 2)))
  return(regrouped.x)
}

#' Requires a given package and if not present installs and loads it.
#'
#' @param package Character name of a package.
#' @param ... Additional arguments passed on to \code{\link{install.packages}}.
#'
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#'
#' @importFrom utils install.packages
#'
#' @export
#'
require_package <- function(package, ...) {
  if (suppressWarnings(!require(package, character.only=TRUE, quietly=TRUE))) {
    parent.function <- sys.calls()[[1]][1]
    message(paste("Function ", parent.function, " requires package: ", package,
                  ". Package not found, installing...", sep=""))
    install.packages(package, ...) # Install the packages
    require(package, character.only=TRUE) # Remember to load the library after installation
  }
}

#' Get the directory of Zonation tutorial.
#'
#' @return path Character path to Zonation tutorial directory.
#'
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export
#'
get_tutorialdir <- function() {
  return(get_options()$tutorial.dir)
}

#' Get subset of curves/group curves columns.
#'
#' Function gets a single column (feature/group/stat) from curves or group
#' curves. Useful for construction melt-type data frames for plotting.
#'
#' @param x ZCurvesDataFrame or ZGroupCurvesDataFrame object
#' @param stat character string of the statistic used ['min', 'mean', 'max',
#' 'w.mean', 'ext2'].
#' @param name character name of a group/feature.
#' @param size numeric defining line width.
#' @param lty integer defining line type.
#'
#' @return data frame for a single column in curves / group curves data.
#'
#' @keywords internal
#'
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#'
sub_curves <- function(x, stat, name, size=0.6, lty=1) {
  col.name <- paste0(stat, '.', name)
  sub.curves <- curves(x, cols=col.name, groups=TRUE)
  names(sub.curves) <- c('pr_lost', 'value')
  sub.curves$name <- name
  sub.curves$stat <- stat
  # We need to coerce Zcurves to a data frame here for later rbinding
  return(data.frame(sub.curves))
}

#' Get unique group names.
#'
#' Method extracts group names directly from group curves data frame header
#' based on a hard-coded set of prefixes
#'
#' @param x data frame groups data.
#'
#' @return character vector of unique group names
#'
#' @keywords internal
#'
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#'
unique_grp_names <- function(x) {
  # Leave pr.lost and cost out
  group.names <- names(x)[-c(1, 2)]
  # Since group name can be whatever, just replace the known header prefixes
  # with nothing
  prefixes <- '(min\\.|mean\\.|max\\.|w\\.mean\\.|ext2\\.)'
  group.names <- gsub(prefixes, "", group.names)
  group.names <- unique(group.names)

  return(group.names)
}

#' Get various Zonation legends
#'
#' Zonation result rank rasters can be displayed in various color schemes.
#'
#' Each color scheme is a list with following item:
#'
#' \describe{
#'    \item{\code{values}:}{Value breaks in the rank priority map}
#'    \item{\code{labels}:}{Labels to be used in the map legend}
#'    \item{\code{colors}:}{Colors used for the value classes}
#'  }
#'
#' Following color schemes are available:
#'
#' \enumerate{
#'    \item{"spectral"}
#' }
#'
#' @param x String character name for the color scheme.
#'
#' @return A list color scheme.
#'
#' @note Color schemes are stored in env \code{.options}.
#'
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export
#' @examples
#' zlegend("spectral")
#'
zlegend <- function(x) {

  if (x == "spectral") {
    return(.options$z_colors_spectral)
  } else if (x == "BrBG") {
    return(.options$z_colors_BrBG)
  } else {
    stop("No legend scheme ", x, " defined")
  }
}
