# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomäki <joona.lehtomaki@gmai.com>. All rights 
# reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' read.ppa.lsm
#' 
#' Read Zonation post-processing analysis (ppa) result file and return a list of
#' 3 items:
#' 1. Most important species in units x
#' 2. Average proportion remaining over all spp in units
#' 3. Data fractions in units
#'
#' @param x input file path
#'
#' @return list of 3 data frames.
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export

read.ppa.lsm <- function(x) {
  con  <- file(x, open = "r")
  
  
  
}