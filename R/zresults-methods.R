# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomäki <joona.lehtomaki@gmai.com>. All rights 
# reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

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

#' @rdname zvariant-methods
#' @aliases curves,Zvariant-method
#' 
setMethod("curves", c("Zresults"), function(x, cols=NULL, groups=FALSE) {
  if (groups) {
    return(x@grp.curves) 
  } else {
    
    if (is.null(cols)) {
      return(x@curves)
    } else {
      # All col names in the curves data
      col.names <- names(x@curves)
      if (is.character(cols)) {
        if (!all(cols %in% col.names)){
          warning(paste("Column names", paste(cols[!cols %in% col.names], 
                              collapse=", "), "not found in curves header"))
          cols <- cols[cols %in% col.names]
          if (length(cols) == 0) {
            return(NA)
          } 
        }
        inds <- sapply(cols, function(y) {which(y == col.names)})
      } else if (is.numeric(cols)) {
        inds <- cols
        if (any(cols < 1)) {
          warning(paste("Column indexes", paste(cols[which(cols < 1)], 
                                              collapse=", "), 
                        "smaller than 1"))
          inds <- cols[which(cols >= 1)]
        }
        ncols <- ncol(x@curves)
        if (any(cols > ncols)) {
          warning(paste("Column indexes", paste(cols[which(cols > ncols)], 
                                                collapse=", "), 
                        "greater than ncol"))
          inds <- cols[which(cols <= ncols)]
        }
      }
      # pr_lost (index = 1) is always included
      if (! 1 %in% inds) {
        inds <- c(1, inds)
      }
      
      feature.curves <- x@curves[inds]
      row.names(feature.curves) <- 1:nrow(feature.curves)
    }
    
      
    return(feature.curves)
  }
})

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

#' @rdname zvariant-methods
#' @aliases performance,Zvariant-method
#' 
setMethod("performance", c("Zresults"), function(x, pr.lost, features=NULL,
                                                 groups=FALSE) {

  if (any(pr.lost < 0 || pr.lost > 1.0)) {
    stop("Proportion landscape lost (pr.lost) values must be 0 < pr.lost < 1")
  }
  perf.data <- curves(x, cols=features)
  # curves() can return a NA if now features are found. If this happens, return
  # NA as well
  if (length(perf.data) < 2) {
    return(NA)
  }
  # [fix] - Ugly
  if (is.null(features)) {
    perf.data <- perf.data[,c(1, 8:ncol(perf.data))]
  }
  row.ids <- sapply(breaks, function(y) {which(perf.data$pr_lost >= y)[1]})
  perf.data <- perf.data[row.ids,]
  row.names(perf.data) <- 1:nrow(perf.data)
  
  return(perf.data)
  
})