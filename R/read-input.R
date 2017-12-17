# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomaki <joona.lehtomaki@gmai.com>. All rights
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

#' Read a features info file.
#'
#' @param infile Character string input file path.
#'
#' @return Data frame of parsed features info data.
#'
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#'
#' @importFrom utils read.table
#'
#' @export
#'
read_features_info <- function(infile) {
  col.names <- c("Weight", "DistSum", "IGRetained",
                 "TviolationFractRem", "DistrMeanX", "DistMeanY",
                 "MapFileName")
  feat.info.dat <- read.table(infile, sep = "\t", skip = 2, as.is = TRUE,
                              col.names = col.names)
  return(feat.info.dat)
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
    read.table(infile, as.is=TRUE, colClasses=rep("numeric", 5))
  }, error=function(cond) {
    message(paste("groups file doesn't seem to contain anything:", infile))
    return(data.frame())
  })

  if (base::ncol(groups.data) != 5) {
    stop("More or less than 5 columns in groups file, check the file:",
            infile)
  }

  if (any(dim(groups.data) != c(0, 0))) {
    names(groups.data) <- c("output.group", "condition.group",
                            "retention.group", "retention.mode",
                            "local.edge.correct.group")
  }
  return(groups.data)
}

#' Read a dat file (Windows-style ini-file) for configuration information.
#'
#' @param infile Character string input file path.
#'
#' @return List of parsed ini-parameters.
#'
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#'
#' @importFrom utils read.table
#'
#' @export
#' @note Adapted from http://bit.ly/11e4Jh0
#'
read_dat <- function(infile) {

    if (!file.exists(infile)) {
      stop("dat-file ", infile, " not found!")
    }

    connection <- file(infile)
    lines  <- readLines(connection)
    close(connection)

    # Try to infer if the file really is an ini file, do this by looking for
    # section headers
    if (!any(grepl("\\[.+\\]", lines))) {
      stop(paste("dat-file", infile, "doesn't seem to have any section headers.",
                 "Check that it is a proper ini-file."))
    }

    # Change section headers
    lines <- chartr("[]", "==", lines)
    # Backward slash will cause problems as well
    lines <- chartr("\\", "/", lines)

    connection <- textConnection(lines)
    dat_data <- read.table(connection, as.is = TRUE, sep = "=", fill = TRUE)
    close(connection)

    # Parameter names can't have whitespaces or dashes, replace with underscores
    dat_data$V1 <- clean_str(dat_data$V1)
    #dat_data$V1 <- chartr(" ", "_", dat_data$V1)
    #dat_data$V1 <- chartr("-", "_", dat_data$V1)

    dat_data$V2 <- clean_str(dat_data$V2)
    #dat_data$V2 <- chartr(" ", "_", dat_data$V2)
    #dat_data$V2 <- chartr("-", "_", dat_data$V2)

    # Location of section breaks
    section_breaks <- dat_data$V1 == ""
    dat_data$V3 <- dat_data$V2[which(section_breaks)[cumsum(section_breaks)]]
    dat_data <- subset(dat_data, dat_data$V1 != "")

    # NOTE: since whitespaces etc. are allowed, attribute accessor must be
    # quoted with backticks.
    to_parse  <- paste("dat_list$`", dat_data$V3, "`$`",  dat_data$V1, "` <- '",
                       dat_data$V2, "'", sep = "")

    dat_list <- list()
    eval(parse(text = to_parse))

    return(dat_list)
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
      dat <- read.table(infile, as.is = TRUE,
                        colClasses = c(rep("numeric", 5), "character"))
    },
    error = function(cond) {
      warning(paste("spp file ", infile, " doesn't seem to contain anything ",
                    "or is malformatted. Error message: ", cond))
      return(data.frame())
    }
  )

  if (any(dim(spp.data) != c(0, 0))) {
    names(spp.data) <- c("weight", "alpha", "bqp", "bqp_p", "cellrem",
                         "filepath")
  }
  return(spp.data)
}
