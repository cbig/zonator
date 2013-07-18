# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomäki <joona.lehtomaki@gmai.com>. All rights 
# reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

read.curves <- function(infile) {
  
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

read.admu.curves <- function(file) {
  
  # First row is (again) malformatted 
  
}

read.grp.curves <- function(file) {
  
  grp.curves <- read.table(file, header=TRUE)
  
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

