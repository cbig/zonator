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

#' The Zproject class
#'
#' \code{Zproject} class represents a Zonation project project, i.e. all the 
#' input and output files and folders.
#' 
#' A project contains one or more variants of particular Zonation analysis 
#' setup. A single variant is represented as an instance of 
#' \code{\link[zonator:Zvariant-class]{Zvariant-class}}.
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{root}:}{Character string path pointing to the root (dir) of
#'      the project.}
#'    \item{\code{variants}:}{List of objects of class 
#'      \code{\link[zonator:Zvariant-class]{Zvariant-class}}.}
#'  }
#'
#' @name Zproject
#' @rdname Zproject-class
#' @aliases Zproject-class
#' @exportClass Zproject
#' @author Joona Lehtomaki <joona.lehtomaki@@gmail.com>
#' 
setClass("Zproject", representation(root = "character", variants = "list"))

# Zonation variant ------------------------------------------------------------

#' Check zvariant object's attributes for consistency.
#' 
#' @param object of class zvariant
#'
#' @return A boolean value TRUE if everything is ok, otherwise a character 
#'   vector of encounterd errors. All encountered warning are printed 
#'   on-the-fly. 
#' 
#' @keywords internal
#' 
#' @note For package internal use only
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
check.variant <- function(object) {
  
  if (class(object) != "zvariant") {
    stop(paste0("Object must be class zvariant (is ", class(object), ")"))
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

#' The Zvariant class
#'
#' \code{Zvariant} class represents a Zonation anaylis variant with the 
#' associated parameters.
#' 
#' Currently \code{Zvariant} must be instantiated based on an existing Zonation
#' batch file. If the variant has been run, then the results are also
#' associated with the instance of \code{Zvariant-class}.
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{name}:}{Character string name of the variant.}
#'    \item{\code{bat.file}:}{Character string path to a Zonation-style batch 
#'      file.}
#'    \item{\code{call.params}:}{Lista of parsed call parameters from the 
#'      batch file.}
#'    \item{\code{results}:}{List holding the results (data frames).}
#'  }
#'
#' @name Zvariant
#' @rdname Zvariant-class
#' @aliases Zvariant-class
#' @exportClass Zvariant
#' @author Joona Lehtomaki <joona.lehtomaki@@gmail.com>
#' 
setClass("Zvariant", representation(name = "character", bat.file = "character",
                                    dat.data = "list", spp.data = "data.frame",
                                    call.params = "list", results = "list"),
         validity = check.variant)