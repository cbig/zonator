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
#' @aliases curves,ZGroupCurvesDataFrame-method
#' 
setMethod("curves", c("ZGroupCurvesDataFrame"), function(x, cols=NULL) {
  
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
    curves.data <- new("ZGroupCurvesDataFrame", curves.data)
    # Update also feature identifier
    curves.data@is.group <- x@is.group[inds]
  }
  return(curves.data)
})

setMethod("groupnames", "ZGroupCurvesDataFrame", function(x) {
  
  if (any(dim(x) == c(0, 0))) {
    return(NA)
  }
  
  # Get all the groups data
  group.data <- x
  
  return(unique_grp_names(group.data))
})

#' Plot Zonation grouped performance curves.
#'
#' @param x data frame containing Zonation's performance curve
#'   (group-specific) output.
#' @param statistic character string indicating which statistic 
#'   (\code{min}, \code{mean}, \code{max}, \code{w.mean}, \code{ext2}) over all 
#'   feature groups is plotted.
#' @param groups integer vector containing the IDs of groups to be
#'   plotted.
#' @param monochrome Boolean indicating if the plot should be in 
#'   monochrome colors only.
#' @param main Character string plot title. 
#' @param invert.x Boolean indicating if the X-axis is printed from 
#'   1 ("feature remaining", \code{FALSE}) or 0 
#'   ("landscape under protection", \code{TRUE}).  
#' @param labels character vector for custom feature labels.
#' @param ... Additional arguments passed on to \code{\link{plot}}.
#' 
#' @seealso \code{\link{read_curves}} and \code{\link{plot_curves}}.
#' 
#' @export
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setMethod("plot", signature(x="ZGroupCurvesDataFrame", y="missing"), 
          function(x, min=FALSE, mean=FALSE, w.mean=FALSE, max=FALSE, ext=FALSE,
                   subs=NULL, monochrome=FALSE, invert.x=FALSE, main="",
                   ...)  {
  
  # If no other stat is provided, set mean to TRUE
  if (!any(min, w.mean, max, ext)) {
    mean <- TRUE
  }
  
  # If no subset is provided, get all groups
  if (length(subs) == 0) {
    grp.names <- groupnames(x)
  } else {
    grp.names <- subs
  }
  # Group curves column is different to that of features, it's:
  # min.GROUP mean.GROUP max.GROUP w.mean.GROUP ext2.GROUP
  selected <- list()
  
  .sub.curves <- function(stat, name, size=0.6, lty=1) {
    col.name <- paste0(stat, '.', name)
    sub.curves <- curves(x, cols=col.name, groups=TRUE)
    names(sub.curves) <- c('pr_lost', 'value')
    sub.curves$group <- name
    sub.curves$stat <- stat
    # We need to coerce Zcurves to a data frame here for later rbinding
    return(data.frame(sub.curves))
  }
  
  for (name in grp.names) {
    if (min) {
      selected[[paste0('min.', name)]] <- .sub.curves('min', name, lty=3)
    }
    if (mean) {
      selected[[paste0('mean.', name)]] <- .sub.curves('mean', name)
    }
    if (max) {
      selected[[paste0('max.', name)]] <- .sub.curves('max', name, lty=2)
    }
    if (w.mean) {
      selected[[paste0('w.mean.', name)]] <- .sub.curves('w.mean', name, lty=4)
    }
    if (ext) {
      selected[[paste0('ext2.', name)]] <- .sub.curves('ext2', name, lty=5)
    }
  }
  
  x.melt <- do.call("rbind", selected)
  row.names(x.melt) <- 1:nrow(x.melt)
  
  p <- plot_curves(x.melt)
  
  #if (monochrome) {
  #  p <- p + theme_bw() + 
  #    scale_colour_grey(name=.options$curve.legend.title)
  #  
  #} else {
  #  p <- p + scale_colour_brewer(name=.options$curve.legend.title,
  #                               palette="Set1")
  #}
  
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
