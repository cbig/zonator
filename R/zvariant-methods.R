# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomäki <joona.lehtomaki@gmai.com>. All rights 
# reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

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

#' @name featurenames<-
#' @rdname zvariant-methods
#' @aliases groupnames<-,Zvariant-method
#' 
setReplaceMethod("featurenames", c("Zvariant", "character"), function(x, value) {
  # Control for length, no cycling allowed
  if (length(value) != nrow(x@spp.data)) {
    stop(paste0("Character vector length (", lenght(value), " and object spp ",
                "data length (", nrow(x@spp.data), " should be the same"))
  }
  x@spp.data$name <- value
  return(x)
})

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
#' @rdname zvariant-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("featurenames", function(x) {
  standardGeneric("featurenames")
})

#' @rdname zvariant-methods
#' @aliases featurenames,Zvariant-method
#' 
setMethod("featurenames", "Zvariant", function(x) {
  
  if (is.na(x@spp.data) || !"name" %in% names(x@spp.data)) {
    stop("No spp data found or it doesn't have 'name' columnd defined")
  }
  return(x@spp.data$name)
})

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

#' @rdname zvariant-methods
#' @aliases groupnames,Zvariant-method
#' 
setMethod("groupnames", "Zvariant", function(x) {
  
  if (is.na(x@groups) || !"name" %in% names(x@groups)) {
    return(NA)
  }
  
  # Get all the groups data
  groups.data <- x@groups
  # Get unique codes
  groups.codes <- unique(groups.data$output.group)
  groups.names <- sapply(groups.codes, function(y) {groups.data[which(groups.data$output.group == y),]$name[1]})
  names(groups.names) <- groups.codes
  return(groups.names)
})

#' Assign group names to a class \code{Zvariant} instance.
#' 
#' This is a replacement function for variant group names. If the particular 
#' variant doesn't use groups the gives a warning.
#'
#' @param x named character vector.
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

#' @name groupnames<-
#' @rdname zvariant-methods
#' @aliases groupnames<-,Zvariant-method
#' 
setReplaceMethod("groupnames", c("Zvariant", "character"), function(x, value) {
  if (empty(x@groups)) {
    stop("Variant has no groups to name")
  }
  # Actual coded values are vector names. Assume numeric and try to coerce.
  keys <- as.numeric(names(value))
  group.codes <- x@groups$output.group
  # Check that the keys actually are found in codes
  if (!all(keys %in% unique(group.codes))) {
    stop(paste("Key(s)", paste(keys[!keys %in% unique(group.codes)], collapse=", "), 
                               "not found in group codes:", 
               paste(group.codes, collapse=", ")))
  }
  # Get the actual character vector indexes based on the names
  inds <- sapply(group.codes, function(y) {which(keys == y)})
  # Index the value vector
  x@groups$name <- value[inds]
  return(x)
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

#' Simple getter mehtod for spp data in a class \code{Zvariant}object.
#' 
#' @param x Zvariant object.
#'
#' @return Data frame (x@spp.data)
#' 
#' @seealso \code{\link{Zvariant-class}}
#' 
#' @export
#' @docType methods
#' @rdname zvariant-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("sppdata", function(x) {
  standardGeneric("sppdata")
})

#' @rdname zvariant-methods
#' @aliases sppdata,Zvariant-method
#' 
setMethod("sppdata", "Zvariant", function(x) {
  return(x@spp.data)
})
