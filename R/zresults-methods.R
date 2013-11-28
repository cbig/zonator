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