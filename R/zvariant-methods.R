# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomäki <joona.lehtomaki@gmai.com>. All rights 
# reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' Get group names of a class \code{Zvariant} instance.
#' 
#' If the particular variant doesn't use groups or doesn't have them assigned, 
#' return NA. Note that here 'groups' means the first column in Zonation groups
#' file ('output group').
#'
#' @param x Zvariant object.
#'
#' @return A factor vector containing the groups. If there are no groups, return
#'         NA.
#' 
#' @seealso \code{\link{Zvariant-class}}
#' 
#' @export
#' @docType methods
#' @rdname zvariant-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("groups", function(x) {
  standardGeneric("groups")
})

#' @rdname zvariant-methods
#' @aliases groups,Zvariant-method
#' 
setMethod("groups", "Zvariant", function(x) {
  if (any(dim(x@groups) != c(0, 0))) {
    return(x@groups$output.group)
  } else {
    return(NA)
  }
})

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

#' @rdname zvariant-methods
#' @aliases has_results,Zvariant-method
#' 
setMethod("has_results", "Zvariant", function(x) {
  # [fix] - Should there be any results even if some are missing?
  if (any(is.na(x@results))) {
    return(FALSE)
  } else {
    return(TRUE)
  }
})

#' Plot Zonation variant's (class \code{Zvariant}) results (performance curves).
#' 
#' In the current implementation, only performance curves (feature-specific or
#' grouped) are plotted. In the future, plot method should handle other types 
#' of results as well.
#' 
#' Method itself is only a thin wrapper to functions 
#' \code{\link{plot_curves}} and \code{\link{plot_grp_curves}}.
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
    plot_grp_curves(x@results[["grp.curves"]], ...)
  } else {
    plot_curves(x@results[["curves"]], ...)
  }
})
