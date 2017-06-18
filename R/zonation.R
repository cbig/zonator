# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomaki <joona.lehtomaki@gmai.com>. All rights
# reserved.

# This program is open source software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' Generate spp_file based on a directory of input rasters.
#'
#' @param filename character string defining the name of the spp file created.
#' @param weight numeric template value for weights.
#' @param alpha numeric template value for alpha values.
#' @param bqp numeric template value for bqp values.
#' @param bqp_p numeric template value for bqp_p values.
#' @param cellrem numeric template value for cellrem values.
#' @param spp_file_dir character path or a vector of paths to target dir.
#' @param recursive Logical defining whether files in \code{spp_file_dir} should
#'   be listed recursively.
#' @param spp_file_pattern pattern used to match raster files.
#' @param override_path character path used to override the dirpath in input
#'   raster file paths. In case \code{recursive = TRUE}, then there can be
#'   an arbitrary number of subdirectories and override path is used only up
#'   until the \code{spp_file_dir}. This way the correct subdirectory structure
#'   is retained.
#'
#' @return invisible(TRUE), functrion is used for side effects.
#'
#' @importFrom utils write.table
#'
#' @export
#'
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#'
create_spp <- function(filename="filelist.spp", weight=1.0, alpha=1.0,
                       bqp=1, bqp_p=1, cellrem=0.25, spp_file_dir,
                       recursive = FALSE,
                       spp_file_pattern = ".+\\.(tif|img)$",
                       override_path = NULL) {

  # List rasters in the target directory. spp_file_dir can be a vector as well,
  # so check for that
  target_rasters <- list()
  if (length(spp_file_dir) > 1) {
    for (item in spp_file_dir) {
      target_rasters[[item]]  <- list.files(path = item,
                                          pattern = spp_file_pattern,
                                          full.names = TRUE,
                                          recursive = recursive)
    }
  } else {
    target_rasters[[spp_file_dir]] <- list.files(path = spp_file_dir,
                                               pattern = spp_file_pattern,
                                               full.names = TRUE,
                                               recursive = recursive)
  }
  if (length(target_rasters) == 0) {
    stop("No raster(s) matching the spp_file_pattern ", spp_file_pattern,
         " found in ", spp_file_dir)
  }

  # Construct the spp file content
  spp_content <- data.frame(weight = weight, alpha = alpha, bqp = bqp,
                            bqp_p = bqp_p, cellrem = cellrem,
                            sppfiles = as.vector(unlist(target_rasters)))

  # Override the target raster paths if needed
  if (!is.null(override_path)) {
    # If recursive TRUE, the override path needs to be modified as the only use
    # case for the override is to act as a prefix for the filename. Using
    # recursive = TRUE can introduce additional levels of nestedness.
    if (recursive) {
      # Again, we may have several spp_file_dirs
      if (length(spp_file_dir) == 1) {
        # Split each path with spp_file_dir. Everything after this will be the
        # subdir path.
        path_components <- strsplit(target_rasters[[1]], paste0(spp_file_dir,
                                                           .Platform$file.sep))
        # Get the last item (subdir path) from each split.
        target_rasters <- sapply(path_components, function(x) x[length(x)])
      } else {
        temp_target_rasters <- c()
        for (target_dir in names(target_rasters)) {
          path_components <- strsplit(target_rasters[[target_dir]],
                                      paste0(target_dir, .Platform$file.sep))
          # Get the last item (subdir path) from each split.
          temp_target_rasters <- c(temp_target_rasters,
                                   sapply(path_components, function(x) x[length(x)]))
        }
        target_rasters <- temp_target_rasters
      }
    } else {
      target_rasters <- sapply(target_rasters, function(x) basename(x))
    }
    spp_content$sppfiles <- file.path(override_path, target_rasters)
  }

  write.table(spp_content, file = filename, row.names = FALSE, quote = FALSE,
              col.names = FALSE)

  return(invisible(TRUE))

}

#' Calculate alpha value for distribution smoothing.
#'
#' alpha-value of biodiversity feature-specific scale of landscape use. The
#' value indicates the range of connectivity of biodiversity features. For
#' example, it may refer to how a species uses the surrounding landscape. This
#' value can be calculated based on, for example, the dispersal capability or
#' the home range sizes of the species.
#'
#' @param landscape.use Use of landscape (in relation to connectivity) in given
#'   map units. Note that if the units used here differ from the real map units
#'   of the biodiversity feature the ratio between two must be set using
#'   \code{ratio} argument.
#' @param ratio Defines the ratio between units used in \code{landscape.use} and
#'   the actual map units in the biodiversity feature. E.g. if the map unit of
#'   a feature is m and use of landscape is defined as 1.5 km, then ratio should
#'   be set to 1000.
#'
#' @return numerical alpha.
#'
#' @seealso Zonation manual.
#'
#' @export
#'
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#'
#' @examples
#'   ds_alpha(1.5, 1000)
#'
ds_alpha <- function(landscape.use, ratio) {
   return(2 / (landscape.use * ratio))
}

#' Check if Zonation is installed.
#'
#' @param exe Character string for overriding the default Zonation executable
#'   (default: zig3).
#'
#' @return A logical indicating whether requested Zonation executable is found.
#'
#' @export
#'
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#'
#' @examples \dontrun{
#'   check_zonation("zig4")
#' }
#'
check_zonation <- function(exe="zig3") {

  if (.Platform$OS.type == "unix") {
    z.exe <- exe
    check <- system(paste("which", z.exe), intern=FALSE, ignore.stdout=TRUE,
                    ignore.stderr=TRUE)
  } else  {
    z.exe <- paste0(exe, ".exe")
    suppressWarnings(check <- shell(z.exe, intern=FALSE, ignore.stdout=TRUE,
                                    ignore.stderr=TRUE))
  }

  if (check == 0) {
    return(TRUE)
  } else {
    warning("Zonation executable ", z.exe, " not found in the system.")
    return(FALSE)
  }
}

#' Parse the content of a Zonation batch (bat) file and make OS specific
#' adjustments if needed.
#'
#' The main issues faced between differerent platforms are the name of the
#' executable, ways of calling it, and path separators. Relative file paths
#' need to be expanded into full absolute paths.
#'
#' @param bat.file Character string path to a Zonation batch (bat) file.
#' @param exe Character string for overriding the Zonation executable
#'   specified in the bat-file.
#'
#' @return A character string command sequence suitable for execution.
#'
#' @export
#'
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#'
parse_bat <- function(bat.file, exe=NULL) {
  # Read in the content of the bat file, surpress any info
  cmd.sequence <- scan(file=bat.file, "character", sep=" ", quiet=TRUE)

  cmd.sequence[4] <- cmd.sequence[4]
  cmd.sequence[5] <- cmd.sequence[5]
  cmd.sequence[6] <- cmd.sequence[6]

  # Which index is the *.exe command at?
  exe.index <- 2

  if (.Platform$OS.type == "unix") {
    # Don't take the 1st element, "call" is specific only to Windows
    if (cmd.sequence[1] == "call") {
      cmd.sequence <- cmd.sequence[2:length(cmd.sequence)]
      exe.index <- 1
    }
    # Remove the *.exe or replace it if needed
    if (is.null(exe)) {
      cmd.sequence[exe.index] <- gsub(".exe", "", cmd.sequence[exe.index])
    } else {
      cmd.sequence[exe.index] <- exe
    }
  } else {
    # Remove the *.exe or replace it if needed
    if (!is.null(exe)) {
      cmd.sequence[exe.index] <- exe
    }
  }
  return(paste(cmd.sequence, collapse=" "))
}

#' Try to run a given batch (bat) files.
#'
#' @param bat.file Character string path to a Zonation batch (bat) file.
#' @param exe Character string for overriding the default Zonation executable
#'   (default: zig3).
#'
#' @return A logical indicating whether running the batch file was successful.
#'
#' @export
#'
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#'
run_bat <- function(bat.file, exe="zig3") {
  if (!file.exists(bat.file)) {
    stop("Bat file ", bat.file, " does not exist.")
  }
  if (check_zonation(exe)) {
    # Temporarily set the working dir to where the bat file is being executed
    # from.
    current.dir <- getwd()
    setwd(dirname(bat.file))
    exit.status <- shell(parse_bat(bat.file, exe))

    setwd(current.dir)

    if (exit.status == 0) {
      return(TRUE)
    } else {
      stop("Running bat file ", bat.file, " failed.")
    }
  }
}
