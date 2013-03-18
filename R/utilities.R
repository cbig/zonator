# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomäki <joona.lehtomaki@gmai.com>. All rights 
# reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' clean.str
#' Clean leading and trailing whitespaces from a given string. Additionally,
#' all occurrences of multiple whitespaces are replaced with a single 
#' whitespace.
#'
#' @param x character string
#'
#' @return A cleaned character string
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export

clean.str <- function(x) {
  
  x <- gsub("\\s+", " ", x)
  
  # returns string w/o leading or trailing whitespace
  x <- gsub("^\\s+|\\s+$", "", x)
  return(x)
}

.line.as.numeric <- function(x) {
  return(as.numeric(.line.as.string(x)))
}

.line.as.string <- function(x) {
  return(unlist(strsplit(x, "\\s+")))
}

open.dir <- function(dir = getwd()){
  if (.Platform['OS.type'] == "windows"){
    shell.exec(dir)
  } else {
    system(paste("dolphin", dir, "&"))
  }
}

require.package <- function(package, ...) {
  if (suppressWarnings(!require(package, character.only=TRUE, quietly=TRUE))) { 
    parent.function <- sys.calls()[[1]][1]
    message(paste("Function ", parent.function, " requires package: ", package,
                  ". Package not found, installing...", sep=""))
    install.packages(package, ...) # Install the packages
    require(package, character.only=TRUE) # Remember to load the library after installation
  }
}