# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomäki <joona.lehtomaki@gmai.com>. All rights 
# reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' Write a Zonation run configuration (dat) file.
#' 
#' Checks a vector of names only contains unique items and if they're not,
#' unique names will be created. Also, the items must be
#' suitable for columns names. Function is strict so that if the vector is not 
#' valid or it cannot be coerced to be one an error is induced. 
#' 
#' @param x Charcter or numeric vector.
#'
#' @return Valid vector of the original size.
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export
#'  
write_dat <- function(x) {

  }