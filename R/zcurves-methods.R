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
#' @aliases curves,Zcurves-method
#' 
setMethod("curves", c("Zcurves"), function(x, cols=NULL) {
  
  if (is.null(cols)) {
    curves.data <- x
  } else {
    col.names <- names(x)

    inds <- map_indexes(cols, col.names)
    
    # Return NA if no inds are found
    if (length(inds) == 0) {
      return(NA)
    }
    
    # pr_lost (index = 1) is always included
    if (! 1 %in% inds) {
      inds <- c(1, inds)
    }
    
    curves.data <- x[inds]

    row.names(curves.data) <- 1:nrow(curves.data)
    curves.data <- new("Zcurves", curves.data, groups=x@groups)
    # Update also feature identifier
    curves.data@is.feature <- x@is.feature[inds]
  }
  return(curves.data)
})

#' @rdname featurenames-methods
#' @aliases featurenames,Zcurves-method
#' 
setMethod("featurenames", signature("Zcurves"), function(x) {
  return(names(x)[x@is.feature])
})

setMethod("groupnames", "Zcurves", function(x) {
  
  if (any(dim(x) == c(0, 0)) || !x@groups) {
    return(NA)
  }
  
  # Get all the groups data
  group.data <- x
  
  return(unique_grp_names(group.data))
})

#' Plot a Zcurves object
#' 
#' Plot a XY line plot of given Zonation performance curves.
#' 
#' @rdname plot-methods
#' @aliases plot_curves,Zcurves,missing,logical-method
#'
setMethod("plot", signature(x="Zcurves", y="missing"), 
          function(x, min=FALSE, mean=FALSE, w.mean=FALSE, max=FALSE,
                   subs=NULL, monochrome=FALSE, invert.x=FALSE, main="",
                   ...)  {
            
  if (x@groups) {
    # If no subset is provided, get all groups
    if (length(subs) == 0) {
      grps <- groupnames(x)
    }
    # Group curves column is different to that of features, it's:
    # min.GROUP mean.GROUP max.GROUP w.mean.GROUP ext2.GROUP
    browser()
    
  } else {
    
    # If no subset is provided, get all features
    if (length(subs) == 0) {
      features <- featurenames(x)
    }
  
    # NOTE! Order matters here.
    if (max) {
      warning("max is not applicable for individual features")
    }
    if (w.mean) {
      features <- c("w_pr", features)
    }
    if (mean) {
      features <- c("ave_pr", features)
    }
    if (min) {
      features <- c("min_pr", features)
    }
    curves.data <- curves(x, cols=features)
  }
  
  # Melt will give a warning here:
  # Setting class(x) to NULL;   result will no longer be an S4 objec
  suppressWarnings(x.melt <- melt(data = curves.data, id.vars=c("pr_lost"), 
                                  measure.vars=2:ncol(curves.data)))
  
  p <- ggplot(x.melt, aes(x=pr_lost, y=value, group=variable))
  p <- p + geom_line(aes(colour = variable), size=1.0)
  
  if (monochrome) {
    p <- p + theme_bw() + 
      scale_colour_grey(name=.options$curve.legend.title)
    
  } else {
    p <- p + scale_colour_brewer(name=.options$curve.legend.title,
                                 palette="Set1")
  }
  
  x.scale <- seq(0, 1, 0.2)
  y.scale <- seq(0, 1, 0.2)
  
  if (invert.x) {
    p <- p + xlab(.options$curve.x.title.invert) + 
             ylab(.options$curve.y.title) +
             scale_x_continuous(breaks=x.scale, labels=1-x.scale) + 
             scale_y_continuous(breaks=y.scale, labels=y.scale)
  } else {
    p <- p + xlab(.options$curve.x.title) + ylab(.options$curve.y.title)
  }
  
  p <- p  + ggtitle(main)
  p <- p + .options$curve.theme
  
  return(p)
})
