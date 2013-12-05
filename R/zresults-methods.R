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
setMethod("curves", c("Zresults"), 
          function(x, cols=NULL, groups=FALSE) {
    
  # Helper function to decide whether given names in indexes exist. Returns 
  # a vector of indexes if names/indexes are actually found.
  .check.names <- function(selected.names, actual.names) {
    if (is.character(selected.names)) {
      if (!all(selected.names %in% actual.names)){
        warning(paste("Column names", 
                      paste(selected.names[!selected.names %in% actual.names], 
                                            collapse=", "), 
                      "not found in curves header"))
        selected.names <- selected.names[selected.names %in% actual.names]
        if (length(selected.names) == 0) {
          return(NULL)
        } 
      }
      inds <- sapply(selected.names, function(y) {which(y == actual.names)})
    } else if (is.numeric(selected.names)) {
      inds <- selected.names
      if (any(selected.names < 1)) {
        warning(paste("Column indexes", 
                      paste(selected.names[which(selected.names < 1)], 
                                              collapse=", "), 
                      "smaller than 1"))
        inds <- selected.names[which(selected.names >= 1)]
      }
      ncols <- length(actual.names)
      if (any(selected.names > ncols)) {
        warning(paste("Column indexes", 
                      paste(selected.names[which(selected.names > ncols)], 
                            collapse=", "), 
                      "greater than ncol"))
        inds <- selected.names[which(selected.names <= ncols)]
      }
    }
    return(inds)
  }
  
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
    inds <- .check.names(cols, col.names)
    
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

#' @rdname Zvariant-methods
#' @aliases plot_curves,Zresults,missing-method
#'
setMethod("plot_curves", signature("Zresults"), 
          function(x, groups=FALSE, min=FALSE, mean=FALSE, w.mean=FALSE, 
                   features=NULL, 
                   monochrome=FALSE, invert.x=FALSE, ...)  {
  if (groups) {
    print("foo")
  } else {
    
    # If no features are provided, get them all
    if (length(features) == 0) {
      features <- featurenames(x)
    }
    
    # NOTE! Order matters here.
    if (w.mean) {
      extras <- c("w_pr", features)
    }
    if (mean) {
      extras <- c("ave_pr", features)
    }
    if (min) {
      features <- c("min_pr", features)
    }
    
    curves.data <- curves(x, cols=features)
  }
  
  x.melt <- melt(data = curves.data, id.vars=c("pr_lost"), 
                 measure.vars=2:ncol(curves.data))
  
  p <- ggplot(x.melt, aes(x=pr_lost, y=value, group=variable))
  p <- p + geom_line(aes(colour = variable), size=1.0)
  
  if (monochrome) {
    p <- p + theme_bw() + 
      scale_colour_grey(name=.options[["curve.legend.title"]])
    
  } else {
    p <- p + scale_colour_brewer(name=.options[["curve.legend.title"]],
                                 palette="Set1", labels=labels)
  }
  
  return(p)
})