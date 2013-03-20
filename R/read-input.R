# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomäki <joona.lehtomaki@gmai.com>. All rights 
# reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' read.ini
#' Read a Windows-style ini-file that is for configuration information.
#'
#' @param infile character string input file path
#'
#' @return a list of parsed ini-parameters
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export
#' @note Adapted from http://bit.ly/11e4Jh0

read.ini <- function(infile) {
    
    connection <- file(infile)
    lines  <- readLines(connection)
    close(connection)
    
    # Change section headers
    lines <- chartr("[]", "==", lines)
    # Backward slash will cause problems as well
    lines <- chartr("\\", "/", lines)
    # If relative paths are used, replace ".." with the absolute path
    lines <- gsub("\\.\\.", dirname(infile), lines)
    
    connection <- textConnection(lines)
    d <- read.table(connection, as.is = TRUE, sep = "=", fill = TRUE)
    close(connection)
    
    # Parameter names can't have whitespaces or dashes, replace with underscores
    d$V1 <- clean.str(d$V1)
    d$V1 <- chartr(" ", "_", d$V1)
    d$V1 <- chartr("-", "_", d$V1)
    
    d$V2 <- clean.str(d$V2)
    d$V2 <- chartr(" ", "_", d$V2)
    d$V2 <- chartr("-", "_", d$V2)
    
    # Location of section breaks
    L <- d$V1 == ""                    
    d <- subset(transform(d, V3 = V2[which(L)[cumsum(L)]])[1:3],
                V1 != "")
    
    ToParse  <- paste("INI.list$", d$V3, "$",  d$V1, " <- '",
                      d$V2, "'", sep="")
    
    INI.list <- list()
    eval(parse(text=ToParse))
    
    return(INI.list) 
}

readWorksheet.disjoint <- function(wb, sheet, regions, ...) {
  
  regions <- unlist(strsplit(regions, ";"))
  
  data.regions <- data.frame()
  
  for (i in 1:length(regions)) {
    if (i == 1) {
      data.regions <- readWorksheet(wb, sheet, region = regions[i], 
                                    header=TRUE)
    } else {
      temp <- readWorksheet(wb, sheet, region = regions[i], header=FALSE)
      # Use colnames fromt the first read
      colnames(temp) <- colnames(data.regions)
      data.regions <- rbind(data.regions, temp)
    }
  }
  
  return(data.regions)
}