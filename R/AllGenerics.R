# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomäki <joona.lehtomaki@gmai.com>. All rights 
# reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# curves ------------------------------------------------------------------

#' @name curves
#' Get curves data of a given \code{Zresults} object.
#' 
#' pr_lost is always included in the returned data, but other columns can be 
#' specified using \code{cols} argument.
#' 
#' @param x \code{Zresults} object.
#' @param cols numeric or character vector of columns to be returned 
#' @param groups logical indicating whether group curves data should be 
#' returned.
#'
#' @return Data frame containing the (selected) curves file data. If column 
#' names are provided, but none are found, return NA.
#' 
#' @seealso \code{\link{Zresults-class}} \code{\link{read_curves}} 
#' \code{\link{read_grp_curves}}
#' 
#' @export
#' @docType methods
#' @rdname zvariant-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("curves", function(x, cols=NULL, groups=FALSE) {
  standardGeneric("curves")
})

# get_variant -------------------------------------------------------------

#' @name get_variant
#' Get a specified variant in a Zonation project
#'
#' @param x Zproject object.
#' @param index int or string index defining the variant required.
#'
#' @return Zvariant object
#' 
#' @seealso \code{\link[zonator:Zproject-class]{Zproject-class}} 
#'   and \code{\link[zonator:Zvariant-class]{Zvariant-class}}
#' 
#' @export
#' @docType methods
#' @rdname zproject-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("get_variant", function(x, index) {
  standardGeneric("get_variant")
})

# nvariants ---------------------------------------------------------------

#' @name nvariants
#' Get the number of variants included in a Zonation project
#'
#' @param x Zproject object.
#'
#' @return int number of variants
#' 
#' @seealso \code{\link{Zproject-class}} and \code{\link{Zvariant-class}}
#'
#' @export
#' @docType methods
#' @rdname zproject-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#'
setGeneric("nvariants", function(x) {
  standardGeneric("nvariants")
})

# opendir -----------------------------------------------------------------

#' @name opendir
#' Open the directory of a Zproject using the system file browser.
#' 
#' Currently support Windows Explorer (Windows) amd Dolphin (Linux/KDE).
#'
#' @param x object.
#'
#' @return invisible
#' 
#' @seealso \code{\link{Zproject-class}} and \code{\link{Zvariant-class}}
#'
#' @export
#' @docType methods
#' @rdname zproject-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("opendir", function(object) { 
  standardGeneric("opendir")
})

# performance -------------------------------------------------------------

#' @name performance
#' Get performance levels either for features or groups from a \code{Zresults} 
#' object.
#' 
#' @param x \code{Zresults} object.
#' @param pr.lost numeric vector containing the fractions of landscape lost
#' for which the feature/group performance values are wanted (default: 'all').
#' @param features character vector of features names to be extracted. Must 
#' match with feature names in curves data
#' @param groups logical indicating whether group curves data should be 
#' used (default: FALSE).
#'
#' @return Data frame containing the curves file data for selected fractions
#' of landscape lost. First column is pr_lost. If feature names are provided and 
#' none are viable, return NA.
#' 
#' @seealso \code{\link{Zresults-class}} \code{\link{read_curves}} 
#' \code{\link{read_grp_curves}}
#' 
#' @export
#' @docType methods
#' @rdname zvariant-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("performance", function(x, pr.lost, features="all", groups=FALSE) {
  standardGeneric("performance")
})

# plot_curves -------------------------------------------------------------

#' @name plot_curves
#' Plot Zonation performance curves.
#' 
#' @param x Zresults object.
#' @param ... Additional arguments passed on to the speficic plotting 
#'   functions.
#'   
#' @return ggplot2 object
#' 
#' @seealso \code{\link{Zvariant-class}}
#' 
#' @export
#' @docType methods
#' @rdname Zresults-methods
#' @aliases plot_curves,Zresults,missing-method
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("plot_curves", function(x, groups=FALSE, min=FALSE, mean=FALSE, 
                                   w.mean=FALSE, 
                                   features=NULL, monochrome=FALSE, 
                                   invert.x=FALSE, ...) {
  standardGeneric("plot_curves")
})
