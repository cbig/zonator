# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomäki <joona.lehtomaki@gmai.com>. All rights 
# reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' A function check feature/group names.
#' 
#' Checks a vector of names only contains unique items and that the items are
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
    stop("All items in names vector must be unique")
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
  
  # Check and replace path separators
  x <- normalizePath(x, mustWork=FALSE)

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
    parent.path <- paste(dir.elements[1:(length(dir.elements)-dot.tokens)], 
                         collapse=.Platform$file.sep)
  }
  
  # Is x valid file path on its own?
  if (file.exists(x)) {
    return(x)
  } else if(!is.null(parent.path)) {
    path <- file.path(parent.path, x)
    # Is x a valid file path when combined with the parent.path?
    if (file.exists(path)) {
      return(path)
    } else {
      # Is the parent path at least valid?
      if (file.exists(parent.path) && !require.file) {
        return(parent.path)
      } else {
        stop("Path ", x, parent.path, " cannot be resolved.")
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
#' @keywords zonation, results
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

#' Requires a given package and if not present installs and loads it.
#' 
#' @param package Character name of a package.
#' @param ... Additional arguments passed on to \code{\link{install.packages}}.
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
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

#' Set the package options to point to a correct location of the Zonation 
#' tutorial
#' 
#' @param x Character string directory path.
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export
#' 
set_tutorialdir <- function(x) {
  if (file.exists(x)) {
    assign("tutorial.dir", x, envir=.option)
  } else {
    warning(paste("Could not set tutorial directory path"), x,
            "Path does not exist.")
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
  return(get("tutorial.dir", .options))
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