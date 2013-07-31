# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomäki <joona.lehtomaki@gmai.com>. All rights 
# reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' check.path
#' Clean leading and trailing whitespaces from a given string. Additionally,
#' all occurrences of multiple whitespaces are replaced with a single 
#' whitespace.
#'
#' @param \code{x} character string file path
#' @param \code{x} character string dir path
#'
#' @return An absolute path to a file of NULL if the path does not exist
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export

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

#' .check.variant
#' Check zvariant object's attributes for consistency.
#' 
#' @param \code{object} of class zvariant
#'
#' @return A boolean value TRUE if everything is ok, otherwise a character 
#'   vector of encounterd errors. All encountered warning are printed 
#'   on-the-fly. 
#' 
#' @keywords internal
#' 
#' @note For package internal use only
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}


.check.variant <- function(object) {
  
  if (class(object) != "zvariant") {
    stop(paste0("Object must be class zvariant (is ", class(object), ")")
  }
  
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

#' clean.str
#' A function to deal with potentially relative paths.
#' 
#' Checks if a path can be resolved (i.e. whether it exists). A additional
#' parameter `parent.path` can be provided, in which case `x` is appended to it
#' and the concatenated path is checked for existence.
#'
#' @param \code{x} character string
#'
#' @return A cleaned character string
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export

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

open.dir <- function(dir = getwd()){
  if (.Platform['OS.type'] == "windows"){
    shell.exec(dir)
  } else {
    system(paste("dolphin", dir, "&"))
  }
}

require.package <- function(package, ...) {
  if (suppressWarnings(!require(package, character.only=TRUE, quietly=TRUE))) { 
    parent.function <- sys.calls()[[1]][1]
    message(paste("Function ", parent.function, " requires package: ", package,
                  ". Package not found, installing...", sep=""))
    install.packages(package, ...) # Install the packages
    require(package, character.only=TRUE) # Remember to load the library after installation
  }
}