# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomäki <joona.lehtomaki@gmai.com>. All rights 
# reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' Create a Zonation project.
#' 
#' Project can be created based on a set of existing bat-files by pointing the 
#' root of he project to an existing directory containing the bat-files. In this
#' case each bat-file is converted into a Zvariant-object.
#' 
#' Alternatively an empty project can be created by supplying a root path that
#' does not exist as an argument.
#'
#' @param root Character string path pointing either to an existing directory
#'   (with potentially bat-files in it) or to a new directory to be created.
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
#' @param debug logical defining if debugging level for logging should be used.
#' @param ... additional arguments passed to \code{\link{create_spp}}.
#'   
#' @return A Zproject object.
#' 
#' @seealso \code{\link[zonator:Zproject-class]{Zproject-class}}, 
#'   \code{\link[zonator:Zvariant-class]{Zvariant-class}} and 
#'   \code{\link[zonator:create_spp]{create_spp}}
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export
#'
create_zproject <- function(root, variants = NULL, dat_template_file = NULL, 
                            spp_template_file = NULL, spp_template_dir = NULL, 
                            debug = FALSE, ...) {
  if (!file.exists(root)) {
    if (is.null(variants)) {
      stop("Root folder provided does not exist and no variant names provided.",
           " Please provide variant names.")
    }
    # Create the new location
    dir.create(root)
    
    # Create an empty README file for the project
    if (debug) message("Creating an empty README file")
    file.create(file.path(root, "README.md"), showWarnings = FALSE)
    
    # Create the variant subfolders with content
    for (variant in variants) {
      variant.dir <- file.path(root, variant)
      dir.create(variant.dir)
      
      # If no templates are provided, use the ones shipped with zonator. Change
      # the filenames to match the variant.
      if (is.null(dat_template_file)) {
        dat_template_file <- system.file("extdata", "template.dat", 
                                         package = "zonator")
      }
      dat.to <- file.path(variant.dir, paste0(variant, ".dat"))
      
      # If no templates are provided, use the ones shipped with zonator. Change
      # the filenames to match the variant.
      if (is.null(spp_template_file) & is.null(spp_template_dir)) {
        spp_template_file <- system.file("extdata", "template.spp", 
                                         package = "zonator")     
      }
      
      # Define the target variant spp file path
      spp.to <- file.path(variant.dir, paste0(variant, ".spp"))
      
      # Copy the templates to the new variant folder
      file.copy(from = dat_template_file, to = dat.to, overwrite = TRUE)
      
      # Work out the deatails depending if using a template file or a 
      # directory of input rasters.
      if (!is.null(spp_template_dir)) {
        if (file.exists(spp_template_dir)) {
          if (debug) {
            message("Creating a spp file from rasters in directory ", 
                    spp_template_dir)
          }
          create_spp(filename = spp.to, spp_file_dir = spp_template_dir, ...)
        }
      } else if (file.exists(spp_template_file)) {
          if (debug) {
            message("Creating a spp file from template ", spp_template_file)
          }
        file.copy(from = spp_template_file, to = spp.to, overwrite = TRUE)
      } else {
        stop("Input template spp file ", spp_template_file, " not found!")
      }
      
      # Create to output folder
      output.dir <- file.path(variant.dir, paste0(variant, "_out"))
      dir.create(output.dir, recursive = TRUE)
      # Create a bat file, first read the template content
      bat.from <- system.file("extdata", "template.bat", package = "zonator")
      cmd.sequence <- scan(file = bat.from, "character", sep = " ", 
                           quiet = TRUE)
      # Replace tokens with actual (relative) paths
      dat.relative <- gsub(paste0(root, .Platform$file.sep), "", dat.to)
      spp.relative <- gsub(paste0(root, .Platform$file.sep), "", spp.to)
      output.dir.relative <- gsub(paste0(root, .Platform$file.sep), "", 
                                  output.dir)
      cmd.sequence <- gsub("INPUT_DAT", dat.relative, cmd.sequence)
      cmd.sequence <- gsub("INPUT_SPP", spp.relative, cmd.sequence)
      cmd.sequence <- gsub("OUTPUT", file.path(output.dir.relative, 
                                                 paste0(variant, ".txt")),
                           cmd.sequence)
      # Write bat-file
      bat.to <- file.path(root, paste0("do_", variant, ".bat"))
      cat(paste(c(cmd.sequence, "\n"), collapse = " "), file = bat.to)
    }
  }
  project <- new("Zproject", root = root, debug = debug)
  return(project)
}