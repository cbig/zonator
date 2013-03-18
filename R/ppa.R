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
  
  dat <- list()
  
  con  <- file(x, open = "r")
  
  # Specify search strings that are used to match the correct header line in the
  # file to the start and end of specific result data chunks. Ending for chunks
  # 1 and 2 is specified by two empty lines.
  
  # Header looks like this:
  # Repeat without spp info for easy import
  # Unit  Area  Mean-Rank  X   Y  Spp_distribution_sum  spp occurring at >10%  >1%  >0.1%  >0.01% >0.001% 
  
  chunk1.start.token <- "Repeat without spp info for easy import"
  
  # Header looks like this:
  # Average proportion remaining over all spp in units = 0.900615
  # Count of species with nothing remaining in the network = 0
  # Total proportion and sum remaining for species
  
  chunk2.start.token <- "Total proportion and sum remaining for species"
  
  # Header looks like this:
  # Biological data of 3991 units.
  # units x species matrix
  # Unit_number  area[cells]  sp_data .....
  
  chunk3.start.token <- "Unit_number"
  
  lines <- readLines(con)
  
  # Placeholders for the actual data
  data1 <- data.frame()
  data2 <- data.frame(stringsAsFactors = FALSE)
  data3 <- data.frame()
  
  # Boolean help variables that control when the data is read in
  read1 <- FALSE
  read2 <- FALSE
  read3 <- FALSE
  
  for (line in lines) {
    
    # 1. Most important species in units x
    if (grepl(chunk1.start.token, line)) {
      # Header included: YES, but at chunk1.start.token + 1 
      # Set read TRUE -> consecutive rows will be read in
      read1 <- TRUE
      next
    }
    
    if (read1) {
      
      # If the header doesn't alrady exist, create it
      if (!exists("header1")) {
        header1 <- c("Unit", "Area", "Mean-Rank", "X", "Y", 
                     "Spp_distribution_sum", "Plus_10", "Plus_1", "Plus_01",
                     "Plus_001", "Plus_0001", "Plus_00001")
        next
      }
      
      if (line != "") { 
        data1 <- rbind(data1, .line.as.numeric(line))
        next
      } else {
        colnames(data1) <- header1
        dat[[1]] <- data1
        read1 <- FALSE
      }
    }

    # 2. Average proportion remaining over all spp in units
    if (grepl(chunk2.start.token, line)) {
      # Header included: NO
      header2 <- c("Feature", "Tot_prop", "Sum")
      read2 <- TRUE
      next
    }
    
    if (read2) {
      
      if (line != "") { 
        data2 <- rbind(data2, .line.as.string(line))
        browser()
        next
      } else {
        
        colnames(data2) <- header2
        dat[[2]] <- data2
        read2 <- FALSE
      }
    }
    
    # 3. Data fractions in units
    if (grepl(chunk3.start.token, line)) {
      # Header included: NO. Header can be constructed only after the first 
      # actual line 
      header3 <- c("Unit_number", "Area_cells")
      read3 <- TRUE
      next
    }
    
    if (read3) {
      if (line != "") { 
        data3 <- rbind(data3, .line.as.numeric(line))
        next
      } else {
        # Finish up the header using the last line of (numeric) data
        nspp <- dim(data3)[2]
        header3 <- c(header3, paste0("Sp_data", 1:nspp))
        colnames(data3) <- header3
        dat[[3]] <- data3
        read3 <- FALSE
      }
    }
  }
  
  close(con)
  return(dat)
}