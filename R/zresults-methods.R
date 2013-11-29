# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomäki <joona.lehtomaki@gmai.com>. All rights 
# reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' Get curves of a given \code{Zresults} object.
#' 
#' @param x \code{Zresults} object.
#' @param groups logical indicating whether group curves data should be 
#' returned.
#'
#' @return Data frame containing the curves file data.
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
setGeneric("curves", function(x, groups=FALSE) {
  standardGeneric("curves")
})

#' @rdname zvariant-methods
#' @aliases curves,Zvariant-method
#' 
setMethod("curves", c("Zresults"), function(x, groups=FALSE) {
  if (groups) {
    return(x@grp.curves) 
  } else {
    return(x@curves)
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
setMethod("performance", c("Zresults"), function(x, pr.lost, features="all",
                                                 groups=FALSE) {

  if (any(pr.lost < 0 || pr.lost > 1.0)) {
    stop("Proportion landscape lost (pr.lost) values must be 0 < pr.lost < 1")
  }
  curves <- curves(x)
  row.ids <- sapply(breaks, function(y) {which(curves$pr_lost >= y)[1]})
  feature.curves <- curves[row.ids, c(1, 8:ncol(curves))]

  if (features[1] != "all") {
    #browser()
    feature.names <- names(feature.curves)
    if (!all(features %in% feature.names)){
      warning(paste("Feature names", 
                    paste(features[!all(features %in% feature.names)], 
                          collapse=", "),
                    "not found in curves header"))
      features <- features[all(features %in% feature.names)]
      if (length(features) == 0) {
        return(NA)
      }
    }
    inds <- sapply(features, function(y) {which(y == feature.names)})
    feature.curves <- feature.curves[c(1, inds)]
    row.names(feature.curves) <- 1:nrow(feature.curves)
  }
  return(feature.curves)
  
})