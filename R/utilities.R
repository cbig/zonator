# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomäki <joona.lehtomaki@gmai.com>. All rights 
# reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' A function to deal with potentially relative paths.
#' 
#' Checks if a path can be resolved (i.e. whether it exists). A additional
#' parameter \code{parent.path} can be provided, in which case \code{x} is 
#' appended to it and the concatenated path is checked for existence.
#'
#' @param x Character string path.
#' @param parent.path Character string root path.
#'
#' @return A cleaned character string
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export
#'  
#' @examples
#'   \dontrun{
#'       
#'   }
#'  
check.path <- function(x, parent.path=NULL) {
  
  # Check and replace path separators
  x <- gsub("\\\\", "/", x)
  
  # Is x valid file path on its own?
  if (file.exists(x)) {
    return(x)
  } else {
    path <- file.path(parent.path, x)
    # Is x a valid file path when combined with the parent.path?
    if (file.exists(path)) {
      return(path)
    } else {
      # Is the parent path at least valid?
      if (file.exists(parent.path)) {
        return(parent.path)
      } else {
        return(NULL)
      }
    }
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
clean.str <- function(x) {
  
  x <- gsub("\\s+", " ", x)
  
  # returns string w/o leading or trailing whitespace
  x <- gsub("^\\s+|\\s+$", "", x)
  return(x)
}

.line.as.numeric <- function(x) {
  return(as.numeric(.line.as.string(x)))
}

.line.as.string <- function(x) {
  return(unlist(strsplit(x, "\\s+")))
}

#' Requires a given package and if not present installs and loads it.
#' 
#' @param package Character name of a package.
#' @param ... Additional arguments passed on to \code{\link{install.packages}}.
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export
#' 
require.package <- function(package, ...) {
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
set.tutorialdir <- function(x) {
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
get.tutorialdir <- function() {
  return(get("tutorial.dir", .options))
}
