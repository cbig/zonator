# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomaki <joona.lehtomaki@gmai.com>. All rights
# reserved.

# This program is open source software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' Read Zonation post-processing analysis (ppa) result file.
#'
#' @param x Character string input file path.
#'
#' @return List of 3 data frames:
#'  1. Most important species in units x
#'  2. Average proportion remaining over all spp in units
#'  3. Data fractions in units
#'
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export
#'
read_ppa_lsm <- function(x) {

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

  chunk2.start.token <- "Total proportion and sum remaining for biodiversity features"

  # Header looks like this:
  # Biological data of 3991 units.
  # units x species matrix
  # Unit_number  area[cells]  sp_data .....

  chunk3.start.token <- "Unit_number"

  lines <- readLines(con)

  index1 <- list("start"=NULL, "end"=NULL)
  index2 <- list("start"=NULL, "end"=NULL)
  index3 <- list("start"=NULL, "end"=NULL)

  line.index <- 1
  for (line in lines) {

    line.index <- line.index + 1

    # 1. Most important species in units x
    if (grepl(chunk1.start.token, line)) {
      # Header included: YES, but at chunk1.start.token + 1
      index1[["start"]] <- line.index + 1
      next
    }

    if (!is.null(index1[["start"]]) & is.null(index1[["end"]]) & line == "") {
      index1[["end"]] <- line.index - 2
      next
    }

    # 2. Average proportion remaining over all spp in uni
    if (grepl(chunk2.start.token, line)) {
      # Header included: NO
      index2[["start"]] <- line.index
      next
    }

    if (!is.null(index2[["start"]]) & is.null(index2[["end"]]) & line == "") {
      index2[["end"]] <- line.index - 2
      next
    }

    # 3. Data fractions in units
    if (grepl(chunk3.start.token, line)) {
      # Header included: NO
      index3[["start"]] <- line.index
      next
    }

    if (!is.null(index3[["start"]]) & is.null(index3[["end"]]) & line.index == length(lines)) {
      index3[["end"]] <- line.index
    }
  }

  if (is.null(index1$start)) {
    stop("PPA start index 1 NULL")
  }

  if (is.null(index1$end)) {
    stop("PPA end index 1 NULL")
  }

  if (is.null(index2$start)) {
    stop("PPA start index 2 NULL")
  }

  if (is.null(index2$end)) {
    stop("PPA end index 2 NULL")
  }

  if (is.null(index3$start)) {
    stop("PPA start index 3 NULL")
  }

  if (is.null(index3$end)) {
    stop("PPA end index 3 NULL")
  }

  .read.chunk <- function(x, start, end, header=NULL, char.rows=FALSE) {

    # Get only the needed rows from the original data x
    if (char.rows) {
      sub.dat <- t(sapply(x[start:end], line_as_string, USE.NAMES=F))
    } else {
      sub.dat <- t(sapply(x[start:end], line_as_numeric, USE.NAMES=F))
    }
    sub.dat <- as.data.frame(sub.dat)
    if (!is.null(header)) {
      colnames(sub.dat) <- header
    }
    return(sub.dat)
  }

  header1 <- c("Unit", "Area", "Mean_rank", "X", "Y", "Spp_distribution_sum",
               "Plus_10", "Plus_1", "Plus_01", "Plus_001", "Plus_0001")

  dat[[1]] <- .read.chunk(lines, index1$start, index1$end, header=header1)
  dat[[1]] <- dat[[1]][-10]

  header2 <- c("Feature", "Tot_prop", "Sum")
  dat[[2]] <- .read.chunk(lines, index2$start, index2$end, header=header2,
                          char.rows=TRUE)
  # For header 3, we need to build it dynamically after the data chunk has been
  # read in
  dat[[3]] <- .read.chunk(lines, index3$start, index3$end)
  header3 <- c("Unit_number", "Area_cells", paste0("F", 1:(ncol(dat[[3]]) - 2)))
  colnames(dat[[3]]) <- header3

  close(con)

  return(dat)
}
