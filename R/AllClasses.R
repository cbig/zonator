# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomaki <joona.lehtomaki@gmai.com>. All rights
# reserved.

# This program is open source software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


# Zonation curves ---------------------------------------------------------

#' The ZCurvesDataFrame class
#'
#' \code{ZCurvesDataFrame} class inherits class data.frame.
#'
#' Class does no implement new methods, but it is used to override some
#' behaviour such as plot. Usually \code{ZCurvesDataFrame} object belongs to a
#' \code{\link[zonator:Zresults-class]{Zresults}} object.
#'
#' @note If user modifies or subsets ZCurvesDataFrame in a funtion it is up to
#' the user to update the indexes in slot \code{is.feature}
#'
#' @section Slots:
#'  \describe{
#'    \item{\code{is.feature}:}{Logical indicating whether column is actually
#'    a feature.}
#'  }
#'
#' @name ZCurvesDataFrame
#' @docType class
#' @rdname ZCurvesDataFrame-class
#' @aliases ZCurvesDataFrame-class
#' @importFrom methods setClass
#' @exportClass ZCurvesDataFrame
#' @author Joona Lehtomaki <joona.lehtomaki@@gmail.com>
#'

setClass('ZCurvesDataFrame', representation(is.feature='logical'),
         contains = 'data.frame')

#' The ZGroupCurvesDataFrame class
#'
#' \code{ZGroupCurvesDataFrame} class inherits class data.frame.
#'
#' Class does no implement new methods, but it is used to override some
#' behaviour such as plot. Usually \code{ZGroupCurvesDataFrame} object belongs
#' to a \code{\link[zonator:Zresults-class]{Zresults}} object.
#'
#' @note If user modifies or subsets ZGroupCurvesDataFrame in a funtion it is up
#' to the user to update the indexes in slot \code{is.group}
#'
#' @section Slots:
#'  \describe{
#'    \item{\code{is.group}:}{Logical indicating whether column is actually
#'    a group.}
#'  }
#'
#' @name ZGroupCurvesDataFrame
#' @docType class
#' @rdname ZGroupCurvesDataFrame-class
#' @aliases ZGroupCurvesDataFrame-class
#' @importFrom methods setClass
#' @exportClass ZGroupCurvesDataFrame
#' @author Joona Lehtomaki <joona.lehtomaki@@gmail.com>
#'

setClass('ZGroupCurvesDataFrame', representation(is.group='logical'),
         contains = 'data.frame')

# Zonation project ------------------------------------------------------------

#' The Zproject class
#'
#' \code{Zproject} class represents a Zonation project project, i.e. all the
#' input and output files and folders.
#'
#' A project contains one or more variants of particular Zonation analysis
#' setup. A single variant is represented as an instance of
#' \code{\link[zonator:Zvariant-class]{Zvariant-class}}.
#'
#'@section Slots:
#'  \describe{
#'    \item{\code{root}:}{Character string path pointing to the root (dir) of
#'      the project.}
#'    \item{\code{variants}:}{List of objects of class
#'      \code{\link[zonator:Zvariant-class]{Zvariant-class}}.}
#'  }
#'
#' @name Zproject
#' @rdname Zproject-class
#' @aliases Zproject-class
#' @importFrom methods setClass
#' @exportClass Zproject
#' @author Joona Lehtomaki <joona.lehtomaki@@gmail.com>
#'
setClass("Zproject", representation(root = "character", variants = "list"))

# Zonation results --------------------------------------------------------

#' Check zvariant object's attributes for consistency.
#'
#' @param object of class zvariant
#'
#' @return A boolean value TRUE if everything is ok, otherwise a character
#'   vector of encounterd errors. All encountered warnings are printed
#'   on-the-fly.
#'
#' @keywords internal
#'
#' @note For package internal use only
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#'
check_results <- function(object) {

  if (class(object) != "Zresults") {
    stop(paste0("Object must be class Zresults (is ", class(object), ")"))
  }

  errors <- character()
  warnings <- character()

  if (!file.exists(object@root)) {
    msg <- paste0("Results root path ", object@root, " does not exist")
    errors <- c(errors, msg)
  }

  if (length(warnings) != 0) {
    for (.warning in warnings) {
      warning(.warning)
    }
  }
  if (length(errors) == 0) TRUE else errors
}

#' The Zresults class
#'
#' \code{Zresults} class represents a full set of Zonation results associated
#' with a single variant (instance of class
#' \code{\link[zonator:Zvariant-class]{Zvariant-class}}).
#'
#' Slots of class \code{Zresults} can be queried using the \code{$}-operator.
#'
#'@section Slots:
#'  \describe{
#'    \item{\code{root}:}{Character string path pointing to the root (dir) of
#'      the results.}
#'    \item{\code{modified}:}{Character timestamp for when results were last modified.}
#'    \item{\code{run.info}:}{Character file path for run info file.}
#'    \item{\code{curves}:}{Data frame of curve (performance) results.}
#'    \item{\code{grp.curves}:}{Data frame of group curve (performance) results.}
#'    \item{\code{rank}:}{RasterLayer object of rank priority.}
#'    \item{\code{wrscr}:}{RasterLayer object of weighted range-size corrected richness.}
#'    \item{\code{prop}:}{RasterLayer object of the proportional loss of distribution.}
#'    \item{\code{ppa.lsm}:}{Data frame containg PPA LSM data items 1 and 3.}
#'    \item{\code{features.info}:}{Data frame containg features info data.}
#'  }
#'
#' @name Zresults
#' @rdname Zresults-class
#' @aliases Zresults-class
#' @exportClass Zresults
#' @author Joona Lehtomaki <joona.lehtomaki@@gmail.com>
#'
setClass("Zresults", representation(root = "character",
                                    modified = "POSIXct",
                                    run.info = "character",
                                    curves = "data.frame",
                                    grp.curves = "data.frame",
                                    rank = "RasterLayer",
                                    wrscr = "RasterLayer",
                                    prop = "RasterLayer",
                                    ppa.lsm = "data.frame",
                                    features.info = "data.frame"),
         validity = check_results)

# Zonation variant ------------------------------------------------------------

#' Check zvariant object's attributes for consistency.
#'
#' @param object of class zvariant
#'
#' @return A boolean value TRUE if everything is ok, otherwise a character
#'   vector of encounterd errors. All encountered warnings are printed
#'   on-the-fly.
#'
#' @keywords internal
#'
#' @note For package internal use only
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#'
check_variant <- function(object) {

  if (class(object) != "Zvariant") {
    stop(paste0("Object must be class Zvariant (is ", class(object), ")"))
  }

  errors <- character()
  warnings <- character()

  # Check the batch file call parameters
  call.params <- object@call.params

  if (is.null(check_path(call.params$dat.file))) {
    msg <- paste0("dat-file ", call.params$dat.file, " cannot be found")
    errors <- c(errors, msg)
  }

  if (is.null(check_path(call.params$spp.file))) {
    msg <- paste0("spp-file ", call.params$spp.file, " cannot be found")
    errors <- c(errors, msg)
  }

  if (is.null(check_path(call.params$dat.file))) {
    msg <- paste0("dat-file ", call.params$dat.file, " cannot be found")
    errors <- c(errors, msg)
  }

  # No need to check output folder as it can be created later. This simply
  # implies that results are not available.

  if (call.params$uc.alpha < 0) {
    # FIXME: is there an upper bound?
    msg <- paste0("Uncertainty parameter alpha cannot be negative: ",
                  call.params$uc.alpha)
    errors <- c(errors, msg)
  }

  if (!call.params$ds.switch %in% c(0, 1)) {
    msg <- paste0("Distribution smoothing switch must be 0 or 1: ",
                  call.params$ds.switch)
    errors <- c(errors, msg)
  }

  if (!call.params$close.window %in% c(0, 1)) {
    msg <- paste0("Close window switch must be 0 or 1: ",
                  call.params$close.window)
    errors <- c(errors, msg)
  }

  if (length(warnings) != 0) {
    for (.warning in warnings) {
      warning(.warning)
    }
  }

  if (length(errors) == 0) TRUE else errors
}

#' The Zvariant class
#'
#' \code{Zvariant} class represents a Zonation analysis variant with the
#' associated parameters.
#'
#' Currently \code{Zvariant} must be instantiated based on an existing Zonation
#' batch file. If the variant has been run, then the results are also
#' associated with the instance of \code{Zvariant-class}.
#'
#'@section Slots:
#'  \describe{
#'    \item{\code{name}:}{Character string name of the variant.}
#'    \item{\code{bat.file}:}{Character string path to a Zonation-style batch
#'      file.}
#'    \item{\code{dat.data}:}{List holding the parsed data from Zonation
#'      dat-file}
#'    \item{\code{spp.data}:}{Data frame holding the parsed data from Zonation
#'      spp-file}
#'    \item{\code{output.dir}:}{Character string path to the output directory.}
#'    \item{\code{groups}:}{Data frame holding the parsed data from Zonation
#'      groups-file}
#'    \item{\code{call.params}:}{List of parsed call parameters from the
#'      batch file.}
#'    \item{\code{condition.layers}:}{Data frame holding the parsed data from
#'      condition file.}
#'    \item{\code{results}:}{List holding the results (data frames).}
#'    \item{\code{results_dirty}:}{Logical indicating if the current object
#'      data (dat.data and spp.data) has been changed when results are present.
#'      If \code{TRUE}, data has changed and results may have been produced
#'      using different data.}
#'  }
#'
#' @name Zvariant
#' @rdname Zvariant-class
#' @aliases Zvariant-class
#' @importFrom methods setClass
#' @exportClass Zvariant
#' @author Joona Lehtomaki <joona.lehtomaki@@gmail.com>
#'
setClass("Zvariant", representation(name = "character", bat.file = "character",
                                    dat.data = "list", spp.data = "data.frame",
                                    output.dir = "character",
                                    groups = "data.frame",
                                    condition.layers = "data.frame",
                                    call.params = "list", results = "Zresults",
                                    results_dirty = "logical"),
         validity = check_variant)
