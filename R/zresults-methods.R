# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomaki <joona.lehtomaki@gmai.com>. All rights
# reserved.

# This program is open source software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' @rdname cost-methods
#' @aliases cost,Zresults-method
#'
setMethod("cost", signature("Zresults"), function(x) {
  if (has_results(x)$curves) {
    cost.data <- x@curves[,1:2]
    names(cost.data) <- c("pr_lost", "cost")
  } else {
    cost.data <- NA
    warning("No results (curves) data available")
  }
  return(cost.data)
})

#' @rdname curves-methods
#' @aliases curves,Zresults-method
#'
setMethod("curves", c("Zresults"), function(x, cols=NULL, groups=FALSE,
                                            lost.lower=0.0, lost.upper=1.0) {

  # Check the lower and upper limits for pr_lost
  if (lost.lower < 0 | lost.lower > 0.99) {
    stop("lost.lower must a numeric value between [0.0, 0.99]")
  }
  if (lost.upper > 1.0 | lost.upper < 0.01) {
    stop("lost.upper must a numeric value between [0.01, 1.0]")
  }
  if (lost.upper <= lost.lower) {
    stop("lost.upper must always be greater than lost.lower")
  }

  if (is.null(cols)) {
    # No specific columns selected, return everything
    if (groups) {
      curves.data <- x@grp.curves
    } else {
      curves.data <- x@curves
    }
    row.inds <- which(curves.data$pr_lost >= lost.lower & curves.data$pr_lost <= lost.upper)
    curves.data <- curves.data[row.inds,]
    if (groups) {
      curves.data <- new("ZGroupCurvesDataFrame", curves.data)
    } else {
      curves.data <- new("ZCurvesDataFrame", curves.data)
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
    row.inds <- which(curves.data$pr_lost >= lost.lower & curves.data$pr_lost <= lost.upper)
    curves.data <- curves.data[row.inds,]
    row.names(curves.data) <- 1:nrow(curves.data)

    if (groups) {
      curves.data <- new("ZGroupCurvesDataFrame", curves.data)
      # Update also feature identifier
      curves.data@is.group <- x@grp.curves@is.group[inds]
    } else {
      curves.data <- new("ZCurvesDataFrame", curves.data)
      curves.data@is.feature <- x@curves@is.feature[inds]
    }
  }
  return(curves.data)
})

#' @rdname featurenames-methods
#' @aliases featurenames,Zresults-method
#'
setMethod("featurenames", signature("Zresults"), function(x) {
  return(names(x@curves)[x@curves@is.feature])
})

#' @rdname features_info-methods
#' @aliases features_info,Zresults-method
#'
setMethod("features_info", c("Zresults"), function(x) {
  if (has_results(x)$features.info) {
    return(x@features.info)
  } else {
    warning("Features info data requested but not present in ", outdir(x))
  }
})

#' @rdname groupnames-methods
#' @aliases groupnames,Zresults-method
#'
setMethod("groupnames", "Zresults", function(x) {

  if (any(dim(x@grp.curves) == c(0, 0))) {
    return(NA)
  }

  # Get all the groups data
  group.data <- x@grp.curves

  return(unique_grp_names(group.data))
})

#' @rdname has_results-methods
#'
setMethod("has_results", "Zresults", function(x) {

  res <- list()

  # Construct results diagnostics based on slot content
  res$curves <- (!length(x@curves) == 0)
  res$grp.curves <- (!length(x@grp.curves) == 0)
  res$rank <- hasValues(x@rank)
  res$wrscr <- hasValues(x@wrscr)
  res$prop <- hasValues(x@prop)
  res$ppa.lsm <- (!length(x@ppa.lsm) == 0)
  res$features.info <- (!length(x@features.info) == 0)

  return(res)
})

#' @rdname outdir-methods
#' @aliases outdir,Zresults-method
#'
setMethod("outdir", c("Zresults"), function(x) {
  return(x@root)
})

#' @rdname ppa_lsm-methods
#' @aliases ppa_lsm,Zresults-method
#'
setMethod("ppa_lsm", signature("Zresults"), function(x) {
  return(x@ppa.lsm)
})

#' @rdname performance-methods
#' @aliases performance,Zresults-method
#'
setMethod("performance", c("Zresults"), function(x, pr.lost, features=NULL,
                                                 groups=FALSE, melted=FALSE) {
  if (any(pr.lost < 0 || pr.lost > 1.0)) {
    stop("Proportion landscape lost (pr.lost) values must be 0 < pr.lost < 1")
  }

  # If groups are used, the names of the groups need to be patched because only
  # mean values are needed
  if (groups && !is.null(features)) {
    features <- paste0("mean.", features)
  }

  perf.data <- curves(x, cols=features, groups=groups)
  # curves() can return a NA if no features are found. If this happens, return
  # NA as well
  if (length(perf.data) < 2) {
    return(NA)
  }

  # Which column index marks the start of the actual feature data
  start <- ifelse(groups, 3, 8)

  # [fix] - Ugly
  if (is.null(features)) {
    perf.data <- perf.data[,c(1, start:ncol(perf.data))]
  }
  row.ids <- sapply(pr.lost, function(y) {which(perf.data$pr_lost >= y)[1]})
  perf.data <- perf.data[row.ids,]

  if (melted) {
    # Transpose feature names as a column and performance levels as consecutive
    # cols
    performances <- list()
    features <- names(perf.data)[2:ncol(perf.data)]

    for (i in 1:length(perf.data$pr_lost)) {
      perf.df <- data.frame(feature=features,
                            pr.lost=pr.lost[i],
                            perf.levels=unlist(perf.data[i,][2:ncol(perf.data)]))
      performances[[i]] <- perf.df
    }
    perf.data <- do.call("rbind", performances)
  }

  row.names(perf.data) <- 1:nrow(perf.data)

  return(perf.data)
})

#' @rdname rank_raster-methods
#' @aliases rank_raster,Zresults-method
#'
setMethod("rank_raster", c("Zresults"), function(x) {
  if(has_results(x)$rank) {
    return(x@rank)
  } else {
    warning("Rank raster requested but not present in ", outdir(x))
  }
})
