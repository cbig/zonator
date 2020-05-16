# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomaki <joona.lehtomaki@gmai.com>. All rights
# reserved.

# This program is open source software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' Compare matrices in various ways.
#'
#' Function can be used to compare two Zonation output rasters with one of the
#' following functions (part of zonator package):
#' \itemize{
#'  \item{correlation}{}
#'  \item{substraction}{}
#'  \item{ferquency}{(NOT IMPLEMENTED)}
#'  \item{coverage}{}
#'  }
#'
#' @keywords post-processing ppa
#' @author Joona Lehtomaki <joona.lehtomaki@@gmail.com>
#'
#' @param x Numeric matrix.
#' @param y Numeric matrix.
#' @param fun Function used for the numeric comparison.
#' @param ... Further arguments passed on to selected comparison function.
#'
#' @return A DataFrame with each row containg columns title, count, and catid
#'
#' @export
#' @seealso correlation substraction frequency coverage

comp <- function(x, y, fun="correlation", ...) {

  # Check the data
  if (!is.matrix(x) | !is.matrix(y)) {
    stop("Both inputs must be matrices.")
  }
  if (dim(x)[1] != dim(y)[1] | dim(x)[2] != dim(y)[2]) {
    stop("Matrix dimensions do not match.")
  }

  switch(fun,
         correlation = correlation(x, y, ...),
         substraction = (x - y),
         # NOT IMPLEMENTED
         # frequency = selection.frequency(x, y, ...),
         coverage = selection_coverage(x, y, ...))
}

#' Correlation between two matrices.
#'
#' Calculate correlation between two matrices using \code{\link{cor}}. A
#' group of specific threshold can be set, in which case the correlations are
#' calculated incrementally for values above the thresholds.
#'
#' @keywords post-processing ppa
#' @author Joona Lehtomaki <joona.lehtomaki@@gmail.com>
#'
#' @param x Numeric matrix.
#' @param y Numeric matrix.
#' @param method Character string correlation method used (default: 'kendall').
#' @param thresholds Numeric vector of thresholds used (default: c(0)).
#'
#' @return A list with 2 items:
#' \item{thresholds}{Correlations between 2 matrices with values above a given threshold.}
#' \item{total}{Overall correlation between the 2 matrices.}
#'
#' @importFrom stats cor
#'
#' @export
#' @seealso \code{\link{cor}}

correlation <- function(x, y, method='kendall', thresholds=c(0)) {

  res <- c()
  for (i in 1:length(thresholds)) {
    x.sel <- as.vector(x[which(x > thresholds[i])])
    y.sel <- as.vector(y[which(y > thresholds[i])])
    res <- append(res, cor(x.sel, y.sel, method=method))
  }

  res <- data.frame(res, row.names=thresholds)
  colnames(res) <- "correlation"
  return(list(thresholds=res, total=cor(as.vector(x), as.vector(y),
                                     method=method)))
}

#' Intersection of two coverages.
#'
#' Calculate how much two coverages (as defined by values greater than a given
#' threshold in two numeric matrices) overlap.
#'
#' @keywords post-processing ppa
#' @author Joona Lehtomaki <joona.lehtomaki@@gmail.com>
#'
#' @param x Numeric matrix.
#' @param y Numeric matrix.
#' @param thresholds Numeric vector of thresholds used.
#'
#' @return A list with 2 items:
#' \item{thresholds}{Correlations between 2 matrices with values above a given threshold.}
#' \item{total}{Overall correlation between the 2 matrices.}
#'
#' @export

selection_coverage <- function(x, y, thresholds) {
  covs <- c()
  total <- c()
  for (thresh in thresholds) {
    sel1 <- which(x >= thresh)
    sel2 <- which(y >= thresh)

    # What fraction of the original coverage x in selection?
    total <- append(total, length(sel1) / length(x))
    covs <- append(covs, sum(sel1 %in% sel2) / length(sel1))
  }

  res <- data.frame(total=total, cover=covs, row.names=thresholds)
  return(res)
}

#' Calculate the Jaccard coefficient.
#'
#' The Jaccard coefficient measures similarity between sample sets, and is
#' defined as the size of the intersection divided by the size of the union of
#' the sample sets. The Jaccard coefficient can be calculated for a subset of
#' rasters provided by using the threshold argument.
#'
#' Min and max values must be provided for both RasterLayer objects \code{x}
#' and \code{y}. Method can be used with RasterLayers of any value range, but
#' the defaults [0.0, 1.0] are geared towards comparing Zonation rank priority
#' rasters. Limits provided are inclusive.
#'
#' @keywords post-processing ppa
#' @author Joona Lehtomaki <joona.lehtomaki@@gmail.com>
#'
#' @param x raster object.
#' @param y raster object.
#' @param x.min Numeric minimum threshold value for \code{x} to be used
#'   (default 0.0).
#' @param x.max Numeric maximum threshold value for \code{x} to be used
#'   (default 1.0).
#' @param y.min Numeric minimum threshold value for \code{y} to be used
#'   (default 0.0).
#' @param y.max Numeric maximum threshold value for \code{y} to be used
#'   (default 1.0).
#' @param warn.uneven Logical indicating whether a warning is raised if the
#'   compared raster coverages are very (>20x) uneven.
#' @param limit.tolerance integer values that defines to which precision x and
#'   y limits are rounded to. This helps e.g. with values that close to 0 but
#'   not quite 0 (default: 4, i.e. round(x, 4)).
#' @param disable.checks logical indicating if the input limit values are
#'   checked against the actual raster values in \code{x} and \code{y}.
#'
#' @return A numeric value [0, 1]
#'
#' @export
#'
jaccard <- function(x, y, x.min=0.0, x.max=1.0, y.min=0.0, y.max=1.0,
                    warn.uneven=FALSE, limit.tolerance=4,
                    disable.checks=FALSE) {

  if (!disable.checks) {
    # Check the input values
    x.min.value <- round(cellStats(x, stat="min"), limit.tolerance)
    x.max.value <- round(cellStats(x, stat="max"), limit.tolerance)
    y.min.value <- round(cellStats(y, stat="min"), limit.tolerance)
    y.max.value <- round(cellStats(y, stat="max"), limit.tolerance)

    if (x.min < x.min.value) {
      stop(paste0("Minimum threshold value for x ("), x.min, ") smaller than
            the computed minimum value in x (", x.min.value, ")")
    }
    if (x.max > x.max.value) {
      stop(paste0("Maximum threshold value for x ("), x.max, ") smaller than
            the computed maximum value in x (", x.max.value, ")")
    }
    if (x.min >= x.max) {
      stop(paste0("Minimum threshold value for x ("), x.min, ") smaller than
             maximum threshold value for x (", x.max, ")")
    }
    if (y.min < y.min.value) {
      stop(paste0("Minimum threshold value for y ("), y.min, ") smaller than
            the computed minimum value in y (", y.min.value, ")")
    }
    if (y.max > y.max.value) {
      stop(paste0("Maximum threshold value for y ("), y.max, ") smaller than
            the computed maximum value in y (", y.max.value, ")")
    }
    if (y.min >= y.max) {
      stop(paste0("Minimum threshold value for y ("), y.min, ") smaller than
             maximum threshold value for y (", y.max, ")")
    }

    # Comparisons using just the defaults is probably not feasible
    if (x.min == 0.0 & x.max == 1.0 & y.min == 0.0 & y.max == 1.0) {
      warning("Using all the defaults for x and y ranges")
    }
  } else {
    message("Input limit checks disabled")
  }

  # [fixme] - using cellStats(X, "sum") should be safe as we're dealing with
  # binary 0/1 rasters. count() would be preferable, but apparently raster
  # (>= 2.2 at least) doesn't support it anymore.

  # Get the values according to the limits provided
  x.bin <- (x >= x.min & x <=x.max)
  y.bin <- (y >= y.min & y <=y.max)

  if (warn.uneven) {
    x.size <- cellStats(x.bin, "sum")
    y.size <- cellStats(y.bin, "sum")
    # Sort from smaller to larger
    sizes <- sort(c(x.size, y.size))
    if (sizes[2] / sizes[1] > 20) {
      warning("The extents of raster values above the threshhold differ more",
              "than 20-fold: Jaccard coefficient may not be informative.")
    }
  }

  # Calculate the intersection of the two rasters, this is given by adding
  # the binary rasters together -> 2 indicates intersection
  combination <- x.bin + y.bin
  intersection <- combination == 2

  # Union is all the area covered by the both rasters
  union <- combination >= 1

  return(cellStats(intersection, "sum") / cellStats(union, "sum"))
}

#' Calculate Jaccard coefficients bewteen all the RasterLayers within a single
#' RasterStack.
#'
#' This method is a utility method that is intented to be used to compare
#' top-fractions of the landscape. Thus, x.max and y.max for
#' \code{\link{jaccard}} are fixed to 1.0.
#'
#' @keywords post-processing ppa
#' @author Joona Lehtomaki <joona.lehtomaki@@gmail.com>
#'
#' @param stack RasterStack-object.
#' @param thresholds Numeric vector values of thresholds.
#' @param ... additional arguments passed on to \code{\link{jaccard}}.
#'
#' @return Dataframe with Jaccard coefficients between all the RasterLayers.
#'
#' @export
#' @seealso \code{\link{jaccard}}

cross_jaccard <- function(stack, thresholds, ...) {

  all.jaccards <- list()

  for (threshold in thresholds) {
    jaccards <- matrix(nrow=nlayers(stack), ncol=nlayers(stack))
    for (i in 1:nrow(jaccards)) {
      for (j in 1:ncol(jaccards)) {
        if (i == j) {
          jaccards[i, j] <- 1
        } else {
          # See the complement, if it's not NA then the pair has already been
          # compared

          if (is.na(jaccards[j, i])) {
            message(paste0("Calculating Jaccard index for [", threshold, ", ",
                           1.0, "] between ", names(stack[[i]]), " and ",
                           names(stack[[j]])))
            jaccards[i, j] <- jaccard(stack[[i]], stack[[j]],
                                      x.min=threshold, x.max=1.0,
                                      y.min=threshold, y.max=1.0, ...)
          } else {
            jaccards[i, j]  <- jaccards[j, i]
          }
        }
      }
    }
    jaccards <- as.data.frame(jaccards)
    colnames(jaccards) <- names(stack)
    rownames(jaccards) <- names(stack)
    all.jaccards[[as.character(threshold)]] <- jaccards
  }
  return(all.jaccards)
}
