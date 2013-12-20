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
#' @param dat.from Character path to a dat file template. If no template is
#'   specified, uses the template distributed with zonator. Ignored if using
#'   an existing project.
#' @param spp.from Character path to a spp file template. If no template is
#'   specified, uses the template distributed with zonator. Ignored if using
#'   an existing project.
#'   
#' @return A Zproject object.
#' 
#' @seealso \code{\link[zonator:Zproject-class]{Zproject-class}} 
#'   and \code{\link[zonator:Zvariant-class]{Zvariant-class}}
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' @export
#'
create_zproject <- function(root, variants, dat.from=NULL, spp.from=NULL) {
 if (!file.exists(root)) {
    # Create the new location
    dir.create(root)
    
    # Create an empty README file for the project
    file.create(file.path(root, "README.md"))
    
    # Create the variant subfolders with content
    for (variant in variants) {
      variant.dir <- file.path(root, variant)
      dir.create(variant.dir)
      
      # If no templates are provided, use the ones shipped with zonator. Change
      # the filenames to match the variant.
      if (is.null(dat.from)) {
        dat.from <- system.file("extdata", "template.dat", 
                                package="zonator")
      }
      dat.to <- file.path(variant.dir, paste0(variant, ".dat"))
      
      if (is.null(spp.from)) {
        spp.from <- system.file("extdata", "template.spp", 
                                package="zonator")     
      }
      spp.to <- file.path(variant.dir, paste0(variant, ".spp"))
      
      # Copy the templates to the new variant folder
      file.copy(from=dat.from, to=dat.to, overwrite=TRUE)
      file.copy(from=spp.from, to=spp.to, overwrite=TRUE)
      # Create to output folder
      output.dir <- file.path(variant.dir, "output") 
      dir.create(output.dir)
      # Create a bat file, first read the template content
      bat.from <- system.file("extdata", "template.bat", package="zonator")
      cmd.sequence <- scan(file=bat.from, "character", sep=" ", quiet=TRUE)
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
      cat(paste(c(cmd.sequence, "\n"), collapse=" "), file=bat.to)
    }
  }
  project <- new("Zproject", root=root)
  return(project)
}