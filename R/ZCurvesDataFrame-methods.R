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
#' @aliases curves,ZCurvesDataFrame-method
#'
setMethod("curves", c("ZCurvesDataFrame"), function(x, cols=NULL) {

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
    curves.data <- new("ZCurvesDataFrame", curves.data)
    # Update also feature identifier
    curves.data@is.feature <- x@is.feature[inds]
  }
  return(curves.data)
})

#' @rdname featurenames-methods
#' @aliases featurenames,ZCurvesDataFrame-method
#'
setMethod("featurenames", signature("ZCurvesDataFrame"), function(x) {
  return(names(x)[x@is.feature])
})

#' @name featurenames<-
#' @rdname featurenames-methods
#' @aliases featurenames<-,ZCurvesDataFrame,character-method
#'
setReplaceMethod("featurenames", c("ZCurvesDataFrame", "character"),
                 function(x, value) {
  # Check names
  value <- check_names(value)

  if (any(dim(x) != c(0,0))) {
    feat.names <- names(x)[8:ncol(x)]
    if (length(value) != length(feat.names)) {
      stop(paste0("Character vector length (", length(value), ") and ",
                  "curves header length (", length(feat.names),
                  ") should be the same"))
    }
    names(x) <- c(names(x)[1:7], value)
  }
  return(x)
})

#' Plot Zonation performance curves for individual features.
#'
#' Generic plotting function for plotting feature performance curves. The method
#' does some data pre-processing specific to \code{\link{ZCurvesDataFrame}}
#' object before passing the data and arguments for \code{\link{plot_curves}}.
#'
#' @note If no other statistic is selected, \code{mean} will be set to TRUE and
#' plotted.
#'
#' @param x \code{\link{ZCurvesDataFrame}} object.
#' @param min logical plot the minimum feature performance of a group
#' (default: FALSE).
#' @param mean logical plot the minimum feature performance of a group
#' (default: FALSE). If no other statistic is used, mean will always be plotted.
#' If other satistic(s) are plotted and mean is to be disabled, this will have
#' to be done by setting \code{mean} explicitly to FALSE.
#' @param w.mean logical plot the weighted mean feature performance of a group
#' (default: FALSE).
#' @param ext logical plot extinction risk of a group (default: FALSE).
#' @param subs character vector defining the names of features (subset of all
#' features) to be plotted.
#'
#' @param ... Additional arguments passed on to \code{\link{plot_curves}}.
#'
#' @seealso \code{\link{read_curves}} and \code{\link{plot_curves}}.
#'
#' @import reshape2
#' @export
#'
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#'
setMethod("plot", signature(x="ZCurvesDataFrame", y="missing"),
          function(x, min=FALSE, mean=FALSE, w.mean=FALSE, ext=FALSE,
                   subs=NULL, ...)  {

  # If no subset is provided, get all features
  if (length(subs) == 0) {
    selected <- featurenames(x)
  } else {
    selected <- subs
  }

  curves.data <- curves(x, cols=selected)

  # Melt will give a warning here:
  # Setting class(x) to NULL;   result will no longer be an S4 objec
  suppressWarnings(x.melt <- reshape2::melt(data = curves.data,
                                            id.vars=c("pr_lost"),
                                            measure.vars=2:ncol(curves.data)))
  x.melt$stat <- "feature"

  # Get possible extra stats fields
  if (w.mean) {
    w.mean.curves <- curves(x, cols="w_pr")
    w.mean.curves <- data.frame(pr_lost=w.mean.curves$pr_lost,
                                variable="Weighted avr prop over all feats",
                                value=w.mean.curves$w_pr,
                                stat="w.mean")
    x.melt <- rbind(w.mean.curves, x.melt)
  }
  if (mean) {
    mean.curves <- curves(x, cols="ave_pr")
    mean.curves <- data.frame(pr_lost=mean.curves$pr_lost,
                              variable="Avr prop over all feats",
                             value=mean.curves$ave_pr, stat="mean")
    x.melt <- rbind(mean.curves, x.melt)
  }
  if (min) {
    min.curves <- curves(x, cols="min_pr")
    min.curves <- data.frame(pr_lost=min.curves$pr_lost,
                             variable="Minimum prop of all feats",
                             value=min.curves$min_pr, stat="min")
    x.melt <- rbind(min.curves, x.melt)
  }
  # Rename 'variable' to 'name'. This is needed to retain compatiblity
  # with plot_curves
  names(x.melt) <- c('pr_lost', 'name', 'value', 'stat')

  p <- plot_curves(x.melt, legend.title=.options$curve.legend.title, ...)

  return(p)
})
