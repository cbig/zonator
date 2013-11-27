# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomäki <joona.lehtomaki@gmai.com>. All rights 
# reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# [todo] - Rename file read-input.R into somehing more appropriate

#' Read Zonation-specific (MS Windows) batch file.
#' 
#' Batch files include calls to Zonation core and look like following:
#' call zig3.exe -r [INPUT_PATH].dat [INPUT_PATH].spp [OUTPUT_PATH].txt 0.0 0 1.0 0 
#'
#' @param infile Character string input file path.
#'
#' @return List of parsed bat-parameters.
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export

read_bat <- function(infile) {
  
  connection <- file(infile)
  lines  <- readLines(connection)
  close(connection)
  
  # FIXME! batch file can have more than 1 call. For now, only 1 row is 
  # supported.
  if (length(lines) > 1) {
    warning(paste0("More than 1 rows defined in bat-file ", infile, 
                   ", using just the first row."))
  }
  
  call.items <- unlist(strsplit(lines, " "))
  
  # FIXME! Now the items are selected based on location, it would be better to
  # parse the call based on regexp with some additional checking.
  bat.list <- list()
  bat.list[["exe"]] <- call.items[2]
  # In practice either "-r" for a new solution or "-l" for loading an existing 
  # solution
  bat.list[["exe.switches"]] <- c(call.items[3])
  # Check for the existence of these input files as they are needed in any case
  bat.list[["dat.file"]] <- check_path(call.items[4], dirname(infile))
  bat.list[["spp.file"]] <- check_path(call.items[5], dirname(infile))
  # Don't validate the output folder path; it can exist of not, depending on 
  # whether the project has been created or not.
  output.folder <- file.path(dirname(infile), call.items[6])
  bat.list[["output.folder"]] <- dirname(gsub("\\\\", "/", output.folder))
  # Uncertainty parameter alpha
  bat.list[["uc.alpha"]] <- as.numeric(call.items[7])
  # Is distribution smoothing used
  bat.list[["ds.switch"]] <- as.numeric(call.items[8])
  # DS kernel multiplier for all features
  bat.list[["alpha.multiplier"]] <- as.numeric(call.items[9])
  # Will the window be closed?
  bat.list[["close.window"]] <- as.numeric(call.items[10])
  
  return(bat.list)
}

#' Read a groups file.
#'
#' @param infile Character string input file path.
#'
#' @return Data frame of parsed groups parameters.
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export
#' 
read_groups <- function(infile) {
  
  if (!file.exists(infile)) {
    stop(paste("Input groups file does not exist:", infile))
  }
  
  groups.data <- tryCatch({
    dat <- read.table(infile, as.is=TRUE, colClasses=rep("numeric", 5))
  },
                       error=function(cond) {
                         message(paste("groups file doesn't seem to contain anything:", infile))
                         return(data.frame())
                       }
  )
  
  if (base::ncol(dat) != 5) {
    stop("More or less than 5 columns in groups file, check the file")
  }
    
  if (any(dim(groups.data) != c(0, 0))) {
    names(groups.data) <- c("output.group", "condition.group", 
                            "retention.group", "retention.mode", 
                            "local.edge.correct.group")
  }
  groups.data$output.group <- factor(groups.data$output.group)
  return(groups.data)
}

#' Read a Windows-style ini-file that is for configuration information.
#'
#' @param infile Character string input file path.
#'
#' @return List of parsed ini-parameters.
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export
#' @note Adapted from http://bit.ly/11e4Jh0
#'
read_ini <- function(infile) {
    
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
    d$V1 <- clean_str(d$V1)
    d$V1 <- chartr(" ", "_", d$V1)
    d$V1 <- chartr("-", "_", d$V1)
    
    d$V2 <- clean_str(d$V2)
    d$V2 <- chartr(" ", "_", d$V2)
    d$V2 <- chartr("-", "_", d$V2)
    
    # Location of section breaks
    L <- d$V1 == ""                    
    d <- subset(transform(d, V3 = V2[which(L)[cumsum(L)]])[1:3], V1 != "")
    
    ToParse  <- paste("INI.list$", d$V3, "$",  d$V1, " <- '", d$V2, "'", sep="")
    
    INI.list <- list()
    eval(parse(text=ToParse))
    
    return(INI.list) 
}

#' Read Zonation variant specific spp-file.
#'
#' @param infile Character string input file path.
#'
#' @return data.frame of parsed spp data.
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export
#'
read_spp <- function(infile) {
  
  if (!file.exists(infile)) {
    stop(paste("Input spp file does not exist:", infile))
  }
  
  spp.data <- tryCatch({
      dat <- read.table(infile, as.is=TRUE, 
                        colClasses=c(rep("numeric", 5), "character"))
    },
    error=function(cond) {
      message(paste("spp file doesn't seem to contain anything:", infile))
      return(data.frame())
    }
  )
  
  if (any(dim(spp.data) != c(0, 0))) {
    names(spp.data) <- c("weight", "alpha", "bqp", "bqp_p", "cellrem", 
                         "filepath")
  }
  return(spp.data)
}