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

#' Get curves data of a Z* object.
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
#' @rdname curves-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("curves", function(x, cols=NULL, groups=FALSE) {
  standardGeneric("curves")
})

# featurenames ------------------------------------------------------------

#' Feature names a for Zonation variant.
#'
#' Getter function to retrieve names for analysis features used a given 
#' Zonation variant.
#'
#' Argument \code{x} can be an instanve of one the following Z* classes:
#' \itemize{
#'   \item{\code{Zvariant}}
#'   \item{\code{Zresults}}
#' }
#' @param x Z* object.
#'
#' @return Character vector of spp feature names. 
#' 
#' @seealso \code{\link{Zvariant-class}} \code{\link{Zresults-class}} 
#'          \code{\link{groupnames}} \code{\link{groups}} 
#' 
#' @export
#' @docType methods
#' @rdname featurenames-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
#' @examples
#' setup.dir <- system.file("extdata/tutorial/basic", package="zonator")
#' tutorial.project <- create_zproject(setup.dir)
#' variant.caz <- get_variant(tutorial.project, "01_core_area_zonation")
#' 
#' # Feature names for a Zvariant object
#' featurenames(variant.caz)
#' 
#' # Feature names for a Zresults object
#' results.caz <- results(variant.caz)
#' featurenames(results.caz)
#' 
setGeneric("featurenames", function(x) {
  standardGeneric("featurenames")
})

# featurenames<- ----------------------------------------------------------

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
#' @rdname Zvariant-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("featurenames<-", function(x, value) {
  standardGeneric("featurenames<-")
})

# get_variant -------------------------------------------------------------

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
#' @rdname Zproject-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("get_variant", function(x, index) {
  standardGeneric("get_variant")
})

# groups ------------------------------------------------------------------

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
#' @rdname Zvariant-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("groups", function(x) {
  standardGeneric("groups")
})

# groupnames --------------------------------------------------------------

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
#' @rdname Zvariant-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("groupnames", function(x) {
  standardGeneric("groupnames")
})

# groupnames<- ------------------------------------------------------------

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
#' @rdname Zvariant-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("groupnames<-", function(x, value) {
  standardGeneric("groupnames<-")
})

# has_results -------------------------------------------------------------

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
#' @rdname Zvariant-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("has_results", function(x) {
  standardGeneric("has_results")
})

# nvariants ---------------------------------------------------------------

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
#' @rdname Zproject-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#'
setGeneric("nvariants", function(x) {
  standardGeneric("nvariants")
})

# opendir -----------------------------------------------------------------

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
#' @rdname Zproject-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("opendir", function(object) { 
  standardGeneric("opendir")
})

# performance -------------------------------------------------------------

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
#' @rdname Zresults-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("performance", function(x, pr.lost, features="all", groups=FALSE) {
  standardGeneric("performance")
})

# results -----------------------------------------------------------------

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
#' @rdname Zvariant-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("results", function(x) {
  standardGeneric("results")
})

# sppdata -----------------------------------------------------------------

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
#' @rdname Zvariant-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("sppdata", function(x, group.names=FALSE) {
  standardGeneric("sppdata")
})
