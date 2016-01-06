# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomaki <joona.lehtomaki@gmai.com>. All rights
# reserved.

# This program is open source software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' Write a Zonation run configuration (dat) file.
#'
#' The function takes a nested list of values and writes it to a dat file (a
#' Windows style .ini-file).
#'
#' @note Only 1 level of nestedness is accepted.
#'
#' @param x List containing the data to be written.
#' @param filename String file path.
#' @param overwrite Logical indicating whether the file should be overwritten.
#'
#' @return Invisible null.
#'
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export
#' @examples
#' dat <- list("Settings" = list("removal_rule" = 1, use_groups = 1))
#' write_dat(dat, "settings.dat")
#'
write_dat <- function(x, filename, overwrite = FALSE) {

  if (!is.list(x)) {
    stop("Data provided must be a list, not ", class(x))
  }
  # Only one level of nested lists allowed
  for (i in x) {
    for (j in i) {
      if (is.list(j)) {
        stop("Only one level of nested lists allowed.")
      }
    }
  }

  if (file.exists(filename)) {
    if (overwrite) {
      unlink(filename)
    } else {
      stop("File exists, but overwrite is off.")
    }
  }

  for (section_name in names(x)) {
    write(paste0("[", section_name, "]"), file = filename, append = TRUE)
    section_data <- x[[section_name]]
    for (parameter in names(section_data)) {
      write(paste0(parameter, " = ", section_data[[parameter]]),
            file = filename, append = TRUE)
    }
    # If there are more than one section, append with an empty line
    if (length(names(x)) != 1) {
      write("", file = filename, append = TRUE)
    }
  }
  return(invisible(NULL))
}
