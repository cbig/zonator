# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomäki <joona.lehtomaki@gmai.com>. All rights 
# reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' Plot Zonation performance curves.
#' 
#' Method itself is only a thin wrapper to functions 
#' \code{\link{plot_curves}} and \code{\link{plot_grp_curves}}.
#'
#' @param x Zcurves object.
#' @param ... Additional arguments passed on to the speficic plotting 
#'   functions.
#'   
#' @return ggplot object
#' 
#' @seealso \code{\link{Zvariant-class}}
#' 
#' @export
#' @docType methods
#' @rdname Zcurves-methods
#' @aliases plot,Zcurves,missing-method
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#'  
setMethod("plot", signature(x="Zcurves", y="missing"), function(x, ...) {
  if (x@groups) {
    plot_grp_curves(x, ...)
  } else {
    plot_curves(x, ...)
  }
})
