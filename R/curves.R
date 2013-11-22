# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomäki <joona.lehtomaki@gmai.com>. All rights 
# reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' Read in performance curves produced by Zonation.
#' 
#' Header is automatically generated based on the number of features in the 
#' file. If you need to read in grouped curves files, use 
#' \code{\link{read_grp_curves}} instead. 
#' 
#' @keywords zonation, results
#' @author Joona Lehtomaki <joona.lehtomaki@@gmail.com>
#'
#' @param infile Character file path to .curves.txt file.
#'
#' @return A DataFrame with all the information in the curves file. If the 
#'   requested file does not exist, return NA.
#'
#' @export
#' @seealso \code{\link{read_grp_curves}}
#'
read_curves <- function(infile) {
  
  if (is.na(infile)) {
    return(NA)
  }
  
  if(!file.exists(infile)) {
    return(NA)
  }
  
  # Read in the curves file skipping the header line, we'll construct this 
  # later on
  curves <- read.table(infile, as.is=TRUE, header=FALSE, skip=1)
  # Standard header entries
  
  # The header has a set of standard components + proportion for each species
  # remaining at level of removal (created dynamically)
  header <- c("Prop_landscape_lost",           # 1
              "cost_needed_for_top_fraction",  # 2
              "min_prop_rem",                  # 3
              "ave_prop_rem",                  # 4
              "W_prop_rem",                    # 5
              "ext-1",                         # 6
              "ext-2")                         # 7
  
  # Populate the rest of the header lines with sp headers and assign it
  header <- c(header, paste("F", 1:(ncol(curves) - length(header)), sep=""))
  colnames(curves) <- header
  return(curves)
}

#' Read in performance curves for grouped features produced by Zonation.
#' 
#' Header is automatically generated based on the number of groups in the 
#' file. If you need to read in individual curves files, use 
#' \code{\link{read_curves}} instead. 
#' 
#' @keywords zonation, results
#' @author Joona Lehtomaki <joona.lehtomaki@@gmail.com>
#'
#' @param infile Character file path to .curves.txt file.
#'
#' @return A DataFrame with all the information in the curves file. If the 
#'   requested file does not exist, return NA.
#'
#' @export
#' @seealso \code{\link{read_curves}}
#'
read_grp_curves <- function(infile) {
  
  if (is.na(infile)) {
    return(NA)
  }
  
  if(!file.exists(infile)) {
    return(NA)
  }
  
  grp.curves <- read.table(infile, header=TRUE)
  
  # standard part of the header
  header <- c("F.lost", "TF_cost")
  
  # Repeating parts of the group curves header
  rep.header <- c("min", "mean", "max", "w.mean", "ext2")
  times <- (ncol(grp.curves) - length(header)) / length(rep.header)
  rep.header <- paste(rep(rep.header, times), rep(1:times, 
                                                  each=length(rep.header)), 
                      sep="-")
  header <- c(header, rep.header)
  colnames(grp.curves) <- header
  
  return(grp.curves)
}