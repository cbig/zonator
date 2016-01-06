# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomaki <joona.lehtomaki@gmai.com>. All rights
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

#' @rdname groupnames-methods
#' @aliases groupnames,ZGroupCurvesDataFrame-method
#'
setMethod("groupnames", "ZGroupCurvesDataFrame", function(x) {

  if (any(dim(x) == c(0, 0))) {
    return(NA)
  }

  # Get all the groups data
  group.data <- x

  return(unique_grp_names(group.data))
})

#' Plot Zonation performance curves for groups.
#'
#' Generic plotting function for plotting group performance curves. The method
#' does some data pre-processing specific to \code{\link{ZGroupCurvesDataFrame}}
#' object before passing the data and arguments for \code{\link{plot_curves}}.
#'
#' @note If no other statistic is selected, \code{mean} will be set to TRUE and
#' plotted.
#'
#' @param x \code{\link{ZGroupCurvesDataFrame}} object.
#' @param min logical plot the minimum feature performance of a group
#' (default: FALSE).
#' @param mean logical plot the minimum feature performance of a group
#' (default: FALSE). If no other statistic is used, mean will always be plotted.
#' If other satistic(s) are plotted and mean is to be disabled, this will have
#' to be done by setting \code{mean} explicitly to FALSE.
#' @param max logical plot the maxmimun feature performance of a group
#' (default: FALSE).
#' @param w.mean logical plot the weighted mean feature performance of a group
#' (default: FALSE).
#' @param ext logical plot extinction risk of a group (default: FALSE).
#' @param subs character vector defining the names of groups (subset of all
#' groups) to be plotted.
#'
#' @param ... Additional arguments passed on to \code{\link{plot_curves}}.
#'
#' @seealso \code{\link{read_curves}} and \code{\link{plot_curves}}.
#'
#' @export
#'
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#'
setMethod("plot", signature(x="ZGroupCurvesDataFrame", y="missing"),
          function(x, min=FALSE, mean=FALSE, w.mean=FALSE, max=FALSE, ext=FALSE,
                   subs=NULL, ...)  {
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

  for (name in grp.names) {
    if (min) {
      selected[[paste0('min.', name)]] <- sub_curves(x, 'min', name, lty=3)
    }
    if (mean) {
      selected[[paste0('mean.', name)]] <- sub_curves(x, 'mean', name)
    }
    if (max) {
      selected[[paste0('max.', name)]] <- sub_curves(x, 'max', name, lty=2)
    }
    if (w.mean) {
      selected[[paste0('w.mean.', name)]] <- sub_curves(x, 'w.mean', name,
                                                        lty=4)
    }
    if (ext) {
      selected[[paste0('ext2.', name)]] <- sub_curves(x, 'ext2', name, lty=5)
    }
  }

  x.melt <- do.call("rbind", selected)
  row.names(x.melt) <- 1:nrow(x.melt)

  p <- plot_curves(x.melt, legend.title=.options$grp.curve.legend.title,
                   groups=TRUE, ...)

  return(p)
})
