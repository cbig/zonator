# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomaki <joona.lehtomaki@gmai.com>. All rights
# reserved.

# This program is open source software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' Create a new Zonation project on file system.
#'
#' Based on a set of input arguments, creates a new Zonation project on the
#' file system following a particular folder and file layout.
#'
#' @param name Character string name for the project. A new directory named
#'   by \code{name} will be creaated in a location specified by \code{dir}.
#' @param dir Character string path pointing to a location to be created.
#' @param variants Character vector of names for new variants. Ignored if using
#'   an existing project.
#' @param dat_template_file Character path to a dat file template. If no
#'   template is specified, uses the template distributed with zonator. Ignored
#'   if using an existing project.
#' @param spp_template_file Character path to a spp file template. If this or
#'   \code{spp_template_dir} are not specified, uses the template
#'   distributed with zonator. Ignored if using an existing project.
#' @param spp_template_dir Character path to directory containing biodiversity
#'   feature rasters. If this or \code{spp_template_file} are not specified,
#'   uses the template distributed with zonator. If both are defined, then
#'   \code{spp_template_dir} overrides. Ignored if using an existing
#'   project.
#' @param overwrite logical should existing project be overwritten (default:
#'   FALSE).
#' @param debug logical defining if debugging level for logging should be used
#'   (default: FALSE).
#' @param ... additional arguments passed to \code{\link{create_spp}}.
#'
#' @note This function is used only for the intended side-effect of creating a
#'   new Zonation project. To load the project as an instance of
#'   \code{\link[zonator:Zproject-class]{Zproject-class}}, see
#'   \code{\link[zonator:load_zproject]{load_zproject}}.
#'
#' @return Invisible(NULL) .
#'
#' @seealso \code{\link[zonator:load_zproject]{load_zproject}} and
#'   \code{\link[zonator:create_spp]{create_spp}}.
#'
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export
#'
create_zproject <- function(name, dir, variants, dat_template_file = NULL,
                            spp_template_file = NULL, spp_template_dir = NULL,
                            overwrite = FALSE, debug = FALSE, ...) {
  if (!file.exists(dir)) {
    stop("Directory ", dir, " provided does not exist.")
  }

  # Create the new location
  project_dir <- file.path(dir, name)
  if (file.exists(project_dir)) {
    if (overwrite) {
      if (debug) message("Removing existing directory ", project_dir)
      unlink(project_dir, recursive = TRUE, force = TRUE)
    } else {
      stop("Project ", project_dir, " already exists and overwrite is off")
    }
  }

  if (debug) message("Creating a project directory ", project_dir)
  dir.create(project_dir, recursive = TRUE)

  # Create an empty README file for the project
  if (debug) message("Creating an empty README file")
  file.create(file.path(project_dir, "README.md"), showWarnings = FALSE)

  # Create the variant subfolders with content
  for (variant in variants) {
    variant_dir <- file.path(project_dir, variant)
    if (debug) message("Creating a variant directory ", variant_dir)
    dir.create(variant_dir, recursive = TRUE)

    # If no templates are provided, use the ones shipped with zonator. Change
    # the filenames to match the variant.
    if (is.null(dat_template_file)) {
      dat_template_file <- system.file("extdata", "template.dat",
                                       package = "zonator")
    }
    dat_to <- file.path(variant_dir, paste0(variant, ".dat"))

    # If no templates are provided, use the ones shipped with zonator. Change
    # the filenames to match the variant.
    if (is.null(spp_template_file) & is.null(spp_template_dir)) {
      spp_template_file <- system.file("extdata", "template.spp",
                                       package = "zonator")
    }

    # Define the target variant spp file path
    spp_to <- file.path(variant_dir, paste0(variant, ".spp"))

    # Copy the templates to the new variant folder
    if (debug) message("Copying template dat-file ", dat_template_file,
                       " to variant directory ", variant_dir)
    if (file.exists(dat_template_file)) {
      file.copy(from = dat_template_file, to = dat_to, overwrite = TRUE)
    } else {
      stop("dat-file template ", dat_template_file, " not found")
    }
    # Work out the details depending if using a template file or a
    # directory of input rasters.
    if (!is.null(spp_template_dir)) {
      # We may have multiple directories
      if (all(sapply(spp_template_dir, function(x) file.exists(x)))) {
        if (debug) {
          if (length(spp_template_dir) > 1) {
            dir_msg <- paste("Creating a spp file from rasters in directories ",
                             paste(spp_template_dir, collapse = ", "))
          } else{
            dir_msg <- paste("Creating a spp file from rasters in directory ",
                             spp_template_dir)
          }
          message(dir_msg)
        }
        create_spp(filename = spp_to, spp_file_dir = spp_template_dir, ...)
      } else {
        stop("Spp template dir ", spp_template_dir, " not found.")
      }
    } else if (!is.null(spp_template_file)) {
      if (file.exists(spp_template_file)) {
        if (debug) {
          message("Copying template spp-file  ", spp_template_file,
                  " to variant directory ", variant_dir)
        }
        file.copy(from = spp_template_file, to = spp_to, overwrite = TRUE)
      } else {
        stop("Input template spp-file ", spp_template_file, " not found!")
      }
    }

    # Create to output folder
    output_dir <- file.path(variant_dir, paste0(variant, "_out"))
    if (debug) message("Creating an output directory ", output_dir)
    dir.create(output_dir, recursive = TRUE)
    # Create a bat file, first read the template content
    bat_from <- system.file("extdata", "template.bat", package = "zonator")
    cmd_sequence <- scan(file = bat_from, "character", sep = " ",
                         quiet = TRUE)
    # Replace tokens with actual (relative) paths
    dat_relative <- gsub(paste0(project_dir, .Platform$file.sep), "", dat_to)
    spp_relative <- gsub(paste0(project_dir, .Platform$file.sep), "", spp_to)
    output_dir_relative <- gsub(paste0(project_dir, .Platform$file.sep), "",
                                output_dir)
    cmd_sequence <- gsub("INPUT_DAT", dat_relative, cmd_sequence)
    cmd_sequence <- gsub("INPUT_SPP", spp_relative, cmd_sequence)
    cmd_sequence <- gsub("OUTPUT", file.path(output_dir_relative,
                                               paste0(variant, ".txt")),
                         cmd_sequence)
    # Write bat-file
    bat_to <- file.path(project_dir, paste0(variant, ".bat"))
    if (debug) message("Writing bat file ", bat_to)
    cat(paste0(paste(cmd_sequence, collapse = " "), "\n"), file = bat_to)
  }

  return(invisible(NULL))
}

#' Load a Zonation project.
#'
#' Loads an existing Zonation project as an object of
#' \code{\link[zonator:Zproject-class]{Zproject-class}}. Individual variants
#' within the Zonation project are parsed into
#' \code{\link[zonator:Zvariant-class]{Zvariant-class}} objects and potential
#' results into \code{\link[zonator:Zresults-class]{Zresults-class}} objects.
#'
#' @param root Character string path pointing to an existing directory
#'   (with potentially bat-files in it) or to a new directory to be created.
#' @param debug logical defining if debugging level for logging should be used.
#'
#' @return A \code{Zproject} object.
#'
#' @seealso \code{\link[zonator:Zproject-class]{Zproject-class}},
#'   \code{\link[zonator:Zvariant-class]{Zvariant-class}} and
#'   \code{\link[zonator:create_zproject]{create_zproject}}
#'
#' @importFrom methods new
#'
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export
#'
load_zproject <- function(root, debug = FALSE) {

  if (file.exists(root)) {
    project <- new("Zproject", root = root, debug = debug)
    return(project)
  } else {
    stop("Root directory ", root, " does not exist")
  }
}
