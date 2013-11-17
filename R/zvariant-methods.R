# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomäki <joona.lehtomaki@gmai.com>. All rights 
# reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' Check if an instance of (class \code{Zvariant}) has results.
#' 
#' If the results are availbale (i.e. variants have been run) then the variant
#' should have a list object containing the results.
#'
#' @param x Zvariant object.
#'
#' @return boolean value
#' 
#' @seealso \code{\link{Zvariant-class}}
#' 
#' @export
#' @docType methods
#' @rdname zvariant-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 

setGeneric("has_results", function(x) {
  standardGeneric("has_results")
})

#' @rdname zproject-methods
#' @aliases has_results,zvariant-method
#' 
setMethod("has_results", signature(x="Zvariant"), function(x) {
  
  if (length(x@results) > 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
  
})

#' Plot Zonation variant's (class \code{Zvariant}) results (performance curves).
#' 
#' In the current implementation, only performance curves (feature-specific or
#' grouped) are plotted. In the future, plot method should handle other types 
#' of results as well.
#' 
#' Method itself is only a thin wrapper to functions 
#' \code{\link{curve.plot}} and \code{\link{curve.group.plot}}.
#'
#' @param x Zvariant object.
#' @param group boolean indicating whether to plot grouped curves.
#' @param ... Additional arguments passed on to the speficic plotting 
#'   functions.
#'
#' @return Zvariant object
#' 
#' @seealso \code{\link{Zvariant-class}}
#' 
#' @export
#' @docType methods
#' @rdname zvariant-methods
#' @aliases plot,Zvariant,missing-method
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#'  
setMethod("plot", signature(x="Zvariant", y="missing"), 
          function(x, group=FALSE, ...) {
  if (group) {
    curve.group.plot(x@results[["grp.curves"]], ...)
  } else {
    curve.plot(x@results[["curves"]], ...)
  }
})
