# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomäki <joona.lehtomaki@gmai.com>. All rights 
# reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' @rdname get_variant-methods
#' @aliases get_variant,Zproject-method
#' 
setMethod("get_variant", c("Zproject", "ANY"), function(x, index) {
  return(x@variants[[index]])
})

#' @rdname nvariants-methods
#' @aliases nvariants,Zproject-method
#' 
setMethod("nvariants", "Zproject", function(x) {
  return(length(x@variants))
})

#' Names of variants in Zproject
#' 
#' Get the names of all the variants within a given \code{\link{Zproject}}.
#' 
#' @rdname names-methods
#' @aliases names,Zproject-method
#' 
setMethod("names", "Zproject", function(x) {
  return(names(x@variants))
})

#' @rdname opendir-methods
#' @aliases opendir,Zproject-method
#'
setMethod("opendir", "Zproject", function(x) {
  if (.Platform['OS.type'] == "windows"){
    shell.exec(x@root)
  } else {
    system(paste("dolphin", x@root, "&"))
  }
})