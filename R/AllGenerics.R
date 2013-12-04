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

# featurenames ------------------------------------------------------------

#' @name featurenames
#' Get spp feature names for a class \code{Zvariant} instance.
#'
#' @param x Zvariant object.
#'
#' @return Character vector of spp feature names. 
#' 
#' @seealso \code{\link{Zvariant-class}} \code{\link{groupnames}} 
#'          \code{\link{groups}} 
#' 
#' @export
#' @docType methods
#' @rdname Zvariant-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("featurenames", function(x) {
  standardGeneric("featurenames")
})

# featurenames<- ----------------------------------------------------------

#' @name "featurenames<-"
#' Assign spp feature names to a class \code{Zvariant} instance.
#' 
#' This is a replacement function for variant spp feature names.
#' 
#' @note spp features have by default names that are derived from the feature
#' raster file path.
#'
#' @param x character vector. Can be named or not.
#'
#' @return A named character vector containing the feature names. If there are 
#'         no groups, return NA.
#' 
#' @seealso \code{\link{Zvariant-class}} \code{\link{featurenames}}
#' 
#' @export
#' @docType methods
#' @rdname zvariant-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("featurenames<-", function(x, value) {
  standardGeneric("featurenames<-")
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

# groups ------------------------------------------------------------------

#' @name groups
#' Get group codes of a class \code{Zvariant} instance.
#' 
#' If the particular variant doesn't use groups or doesn't have them assigned, 
#' return NA. Note that here 'groups' means the first column in Zonation groups
#' file ('output group').
#'
#' @param x Zvariant object.
#'
#' @return A numeric vector containing the groups. If there are no groups, return
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

# groupnames --------------------------------------------------------------

#' @name groupnames
#' Get group names for a class \code{Zvariant} instance.
#'
#' @param x Zvariant object.
#'
#' @return A named character vector containing the group names. If there are no 
#'         groups, return NA.
#' 
#' @seealso \code{\link{Zvariant-class}} \code{\link{groupnames}} 
#'          \code{\link{groups}} 
#' 
#' @export
#' @docType methods
#' @rdname zvariant-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("groupnames", function(x) {
  standardGeneric("groupnames")
})

# groupnames<- ------------------------------------------------------------

#' @name "groupnames<-"
#' Assign group names to a class \code{Zvariant} instance.
#' 
#' This is a replacement function for variant group names. If the particular 
#' variant doesn't use groups the gives a warning.
#'
#' @param x Zvariant object.
#' @param value named character vector.
#'
#' @return A named character vector containing the group names. If there are no 
#'         groups, return NA.
#' 
#' @seealso \code{\link{Zvariant-class}} \code{\link{groupnames}} 
#'          \code{\link{groups}} 
#' 
#' @export
#' @docType methods
#' @rdname zvariant-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("groupnames<-", function(x, value) {
  standardGeneric("groupnames<-")
})

# has_results -------------------------------------------------------------

#' @name has_results
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

# results -----------------------------------------------------------------

#' @name results
#' Getter method for results (\code{Zresults}) in a class 
#' \code{Zvariant} object.
#' 
#' Since not all changes to Zvariant are reflected to its Zresults (e.g. feature
#' and group names) there may quite a lot runtime patching going on.
#' 
#' @param x Zvariant object.
#'
#' @return Zresults object. If variant doesn't have results return NA.
#' 
#' @seealso \code{\link{Zresults-class}}
#' 
#' @export
#' @docType methods
#' @rdname zvariant-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("results", function(x) {
  standardGeneric("results")
})

# sppdata -----------------------------------------------------------------

#' @name sppdata
#' Simple getter mehtod for spp data in a class \code{Zvariant}object.
#' 
#' Method will also return group column with spp data if it exists. 
#' 
#' @param x Zvariant object.
#' @param group.names boolean indicating whether group codes (FALSE) or names
#' (TRUE) are used to indicate group. (default: FALSE)
#'
#' @return Data frame (object's spp.data)
#' 
#' @seealso \code{\link{Zvariant-class}}
#' 
#' @export
#' @docType methods
#' @rdname zvariant-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("sppdata", function(x, group.names=FALSE) {
  standardGeneric("sppdata")
})
