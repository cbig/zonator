# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomäki <joona.lehtomaki@gmai.com>. All rights 
# reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' @rdname curves-methods
#' @aliases curves,Zresults-method
#' 
setMethod("curves", c("Zresults"), function(x, cols=NULL, groups=FALSE) {
  
  if (is.null(cols)) {
    # No specific columns selected, return everything
    if (groups) {
      curves.data <- x@grp.curves
    } else {
      curves.data <- x@curves
    }
  } else {
    # All col names in the curves/group curves data
    if (groups) {
      col.names <- names(x@grp.curves)
    } else {
      col.names <- names(x@curves)
    }
    inds <- map_indexes(cols, col.names)
    
    # Return NA if no inds are found
    if (length(inds) == 0) {
      return(NA)
    }
    
    # pr_lost (index = 1) is always included
    if (! 1 %in% inds) {
      inds <- c(1, inds)
    }
    
    if (groups) {
      curves.data <- x@grp.curves[inds]  
    } else {
      curves.data <- x@curves[inds]
    }
    row.names(curves.data) <- 1:nrow(curves.data)
  }
  return(curves.data)
})

#' @rdname featurenames-methods
#' @aliases featurenames,Zresults-method
#' 
setMethod("featurenames", signature("Zresults"), function(x) {
  return(names(x@curves)[8:ncol(x@curves)])
})

#' @rdname Zresults-methods
#' @aliases performance,Zresults-method
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
  row.ids <- sapply(pr.lost, function(y) {which(perf.data$pr_lost >= y)[1]})
  perf.data <- perf.data[row.ids,]
  row.names(perf.data) <- 1:nrow(perf.data)
  
  return(perf.data)
  
})
