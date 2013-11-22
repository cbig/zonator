# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomäki <joona.lehtomaki@gmai.com>. All rights 
# reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' get_variant
#' Get a specified variant in a Zonation project
#'
#' @param x Zproject object.
#' @param index int or string index defining the variant required.
#'
#' @return Zvariant object
#' 
#' @seealso \code{\link[zonator:Zproject-class]{Zproject-class}} 
#'   and \code{\link[zonator:Zvariant-class]{Zvariant-class}}
#' 
#' @export
#' @docType methods
#' @rdname zproject-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("get_variant", function(x, index) {
  standardGeneric("get_variant")
})

#' @rdname zproject-methods
#' @aliases get_variant,Zproject-method
#' 
setMethod("get_variant", c("Zproject", "ANY"), function(x, index) {
  return(x@variants[[index]])
})

#' nvariants
#' Get the number of variants included in a Zonation project
#'
#' @param x Zproject object.
#'
#' @return int number of variants
#' 
#' @seealso \code{\link{Zproject-class}} and \code{\link{Zvariant-class}}
#'
#' @export
#' @docType methods
#' @rdname zproject-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#'
setGeneric("nvariants", function(x) {
  standardGeneric("nvariants")
})

#' @rdname zproject-methods
#' @aliases nvariants,Zproject-method
#' 
setMethod("nvariants", "Zproject", function(x) {
  return(length(x@variants))
})

#' @rdname zproject-methods
#' @aliases names,Zproject-method
#' 
setMethod("names", "Zproject", function(x) {
  return(names(x@variants))
})

#' opendir
#' Open the directory of a Zproject using the system file browser.
#' 
#' Currently support Windows Explorer (Windows) amd Dolphin (Linux/KDE).
#'
#' @param x object.
#'
#' @return invisible
#' 
#' @seealso \code{\link{Zproject-class}} and \code{\link{Zvariant-class}}
#'
#' @export
#' @docType methods
#' @rdname zproject-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("opendir", function(object) { 
  standardGeneric("opendir")
})

#' @rdname zproject-methods
#' @aliases opendir,Zproject-method
#'
setMethod("opendir", "Zproject", function(object) {
  if (.Platform['OS.type'] == "windows"){
    shell.exec(object@root)
  } else {
    system(paste("dolphin", object@root, "&"))
  }
})