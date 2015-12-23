# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomäki <joona.lehtomaki@gmai.com>. All rights 
# reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# curves ------------------------------------------------------------------

#' Get curves results data of a Z* object.
#' 
#' pr_lost is always included in the returned data, but other columns can be 
#' specified using \code{cols} argument.
#' 
#' Arguments \code{upper} and \code{lower} can be used to define a specific 
#' range of pr_lost to be returned.
#' 
#' Method implementation in class \code{\link{Zvariant}} is just a thin
#' wrapper for passing the argumets to variant's code{\link{Zresults}} object.
#' 
#' @param x Z* object.
#' @param lost.upper numeric defining the upper limit of pr_lost to be included
#' [0.01, 1.0] (default: 1.0)
#' @param lost.lower numeric defining the lower limit of pr_lost to be included
#' [0.0, 0.99] (default: 0.0)
#' @param cols numeric or character vector of columns to be returned 
#' @param groups logical indicating whether group curves data should be 
#' returned.
#'
#' @return \code{\link{ZCurvesDataFrame}} or \code{\link{ZGroupCurvesDataFrame}} 
#'   containing the (selected) curves file data. If column names are provided, 
#'   but none are found, return NA.
#' 
#' @keywords results
#'  
#' @seealso \code{\link{Zresults-class}} \code{\link{Zvariant-class}}
#' \code{\link{read_curves}} \code{\link{read_grp_curves}}
#' 
#' @export
#' @rdname curves-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("curves", function(x, cols=NULL, groups=FALSE, lost.lower=0.0, 
                              lost.upper=1.0) {
  standardGeneric("curves")
})

# featurenames ------------------------------------------------------------

#' Feature names of Zonation variant.
#'
#' Get and set names for analysis features used a given 
#' Zonation variant.
#'
#' Argument \code{x} can be an instance of one the following Z* classes:
#' \itemize{
#'   \item{\code{Zvariant}}
#'   \item{\code{Zresults}}
#' }
#' 
#' @note spp features have by default names that are derived from the feature
#' raster file path.
#' 
#' @param x Z* object.
#'
#' @return Character vector of spp feature names. 
#' 
#' @seealso \code{\link{Zvariant-class}} \code{\link{Zresults-class}} 
#'          \code{\link{groupnames}} \code{\link{groups}} 
#' 
#' @export
#' @rdname featurenames-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
#' @examples \dontrun{
#'  setup.dir <- system.file("extdata/tutorial/basic", package="zonator")
#'  tutorial.project <- create_zproject(setup.dir)
#'  variant.caz <- get_variant(tutorial.project, "01")
#' 
#'  # Feature names for a Zvariant object
#'  featurenames(variant.caz)
#' 
#'  # Feature names for a Zresults object
#'  results.caz <- results(variant.caz)
#'  featurenames(results.caz)
#' }
#' 
setGeneric("featurenames", function(x) {
  standardGeneric("featurenames")
})

# featurenames<- ----------------------------------------------------------

#' Assign feature names for a Zonation variant.
#' 
#' @param value character vector of feature names to be assigned. Can be named 
#'   or not.
#'
#' @export
#' @rdname featurenames-methods
#' 
setGeneric("featurenames<-", function(x, value) {
  standardGeneric("featurenames<-")
})


# get_dat_param -----------------------------------------------------------

#' Get a specified run setting parameter value
#' 
#' Essentially these are parameter values corresponding to Zonation dat-files.
#' This function is used to get values of given parameter. In the current 
#' implementation, sections have no meaning. 
#'
#' @param x Zvariant object.
#' @param parameter Character string name of the parameter.
#'
#' @return Character string value of the parameter. If requested parameter is 
#'   a valid Zonation parameter but not used currently, returns NA.
#' 
#' @seealso \code{\link[zonator:Zvariant-class]{Zvariant-class}} and 
#' \code{\link{set_dat_parameter}}. 
#' 
#' @export
#' @rdname get_dat_param-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("get_dat_param", function(x, parameter) {
  standardGeneric("get_dat_param")
})

# get_variant -------------------------------------------------------------

#' Get a specified variant in a Zonation project
#'
#' @param x Zproject object.
#' @param index int or string index defining the variant required.
#'
#' @return Zvariant object
#' 
#' @seealso \code{\link[zonator:Zproject-class]{Zproject-class}} 
#'   and \code{\link[zonator:Zvariant-class]{Zvariant-class}}
#' 
#' @export
#' @rdname get_variant-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("get_variant", function(x, index) {
  standardGeneric("get_variant")
})

# groups ------------------------------------------------------------------

#' Get group codes of a class \code{Zvariant} instance.
#' 
#' If the particular variant doesn't use groups or doesn't have them assigned, 
#' return NA. Note that here 'groups' means the first column in Zonation groups
#' file ('output group').
#'
#' @param x Zvariant object.
#'
#' @return A numeric vector containing the group ids. If there are no groups, 
#'         return NA.
#' 
#' @seealso \code{\link{Zvariant-class}}
#' 
#' @export
#' @rdname groups-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("groups", function(x) {
  standardGeneric("groups")
})

# groups<- ----------------------------------------------------------------

#' Assign group codes for a class \code{Zvariant} instance.
#'
#' @param value numeric vector of group ids. Vector length must match to the
#'        number of features, no recycling is done.
#'
#' 
#' @export
#' @rdname groups-methods
#' 
setGeneric("groups<-", function(x, value) {
  standardGeneric("groups<-")
})


# groupnames --------------------------------------------------------------

#' Get group names for a class \code{Zvariant} instance.
#'
#' @param x Zvariant object.
#'
#' @return A character vector containing the group names. If there are no 
#'         groups, return NA.
#' 
#' @seealso \code{\link{Zvariant-class}} \code{\link{groupnames}} 
#'          \code{\link{groups}} 
#' 
#' @export
#' @rdname groupnames-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("groupnames", function(x) {
  standardGeneric("groupnames")
})

# groupnames<- ------------------------------------------------------------

#' Assign group names
#' 
#' Group names can be assigned to a \code{\link{Zvariant}} or 
#' \code{\link{Zresults}} object. This is a replacement function for variant 
#' group names. If the particular variant doesn't use groups the gives a 
#' warning.
#'
#' @param value named character vector.
#'
#' @seealso \code{\link{Zvariant-class}} \code{\link{groupnames}} 
#'          \code{\link{Zresults}} \code{\link{groups}} 
#' 
#' @export
#' @rdname groupnames-methods
#' 
setGeneric("groupnames<-", function(x, value) {  
  standardGeneric("groupnames<-")
})

# has_results -------------------------------------------------------------

#' Check which results a Z* object has.
#' 
#' If the results are availbale (i.e. variants have been run) then the variant
#' should have a list object containing the results. 
#' 
#' The value returned is a list of logical where key of each element corresponds
#' to a specific type of results.
#' 
#' @param x \code{\link[zonator]{Zvariant-class}} or 
#' \code{\link[zonator:Zresults-class]{Zresults}} object.
#'
#' @return list of logical values
#' 
#' @seealso \code{\link{Zvariant-class}}
#' 
#' @export
#' @rdname has_results-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("has_results", function(x) {
  standardGeneric("has_results")
})

# nfeatures ---------------------------------------------------------------

#' Get the number of feature included in a Zonation variant
#'
#' @param x Zvariant object.
#'
#' @return int number of variants
#' 
#' @seealso \code{\link{Zvariant-class}}
#'
#' @export
#' @rdname nfeatures-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#'
setGeneric("nfeatures", function(x) {
  standardGeneric("nfeatures")
})

# nvariants ---------------------------------------------------------------

#' Get the number of variants included in a Zonation project
#'
#' @param x Zproject object.
#'
#' @return int number of variants
#' 
#' @seealso \code{\link{Zproject-class}} and \code{\link{Zvariant-class}}
#'
#' @export
#' @rdname nvariants-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#'
setGeneric("nvariants", function(x) {
  standardGeneric("nvariants")
})

# opendir -----------------------------------------------------------------

#' Open the directory of a Zproject using the system file browser.
#' 
#' Currently support Windows Explorer (Windows) amd Dolphin (Linux/KDE).
#'
#' @param x a \code{\link{Zproject}} object.
#'
#' @return invisible
#' 
#' @seealso \code{\link{Zproject-class}} and \code{\link{Zvariant-class}}
#'
#' @export
#' @rdname opendir-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("opendir", function(x) { 
  standardGeneric("opendir")
})

# outdir ------------------------------------------------------------------

#' Get path to output directory. 
#' 
#' Zonation variant has an output directory defined in project bat-file. This
#' is of course the same path as in the results of the particular variant.
#' 
#' @param x \code{\link[zonator]{Zvariant-class}} or 
#' \code{\link[zonator:Zresults-class]{Zresults}} object.
#'
#' @return character string path to output directory location.
#' 
#' @seealso \code{\link{Zvariant-class}} \code{\link{Zresults-class}}
#' 
#' @export
#' @rdname outdir-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("outdir", function(x) {
  standardGeneric("outdir")
})

# ppa_lsm  ------------------------------------------------------------------

#' Get ppa.lsm results data of a Z* object.
#' 
#' Simple getter-method for ppa.lsm information (if used) contained in Zonation 
#' results
#' 
#' @param x Z* object.
#' 
#' @return Data frame containing PPA LSM data items 1 and 3 combined (See
#' \code{\link{Zresults-class}} for more details). If no results are available, 
#'   give a warning.
#' 
#' @keywords results
#' 
#' @seealso \code{\link{Zresults-class}} \code{\link{read_curves}} 
#' \code{\link{read_grp_curves}}
#' 
#' @export
#' @rdname ppa_lsm-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("ppa_lsm", function(x) {
  standardGeneric("ppa_lsm")
})

# performance -------------------------------------------------------------

#' Get performance levels.
#' 
#' Method returns performance levels at specified levels of cell removal for 
#' particular features (either for individual features or groups from a 
#' \code{Zresults} object). 
#' 
#' @note In the current implementation, only average performance levels for 
#' groups are returned.
#' 
#' @param x \code{Zresults} object.
#' @param pr.lost numeric vector containing the fractions of landscape lost
#' for which the feature/group performance values are wanted (default: 'all').
#' @param features character vector of features names to be extracted. Must 
#' match with feature names in curves data
#' @param groups logical indicating whether group curves data should be 
#' used (default: FALSE).
#' @param melted logical indicating whether the data.frame returned should be in 
#' melted format (default: FALSE)
#'
#' @return Data frame containing the curves file data for selected fractions
#' of landscape lost. First column is pr_lost. If feature names are provided and 
#' none are viable, return NA.
#' 
#' @seealso \code{\link{Zresults-class}} \code{\link{read_curves}} 
#' \code{\link{read_grp_curves}}
#' 
#' @export
#' @rdname performance-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("performance", function(x, pr.lost, features="all", groups=FALSE,
                                   melted=FALSE) {
  standardGeneric("performance")
})

# rank_raster -------------------------------------------------------------

#' Get Zonation result rank raster.
#' 
#' Getter method for rank priority raster included in Zonation results. Rank
#' raster is one of the main outputs of Zonation.
#'  
#' Since a given \code{Zvariant} object can only have 1 rank priority raster,
#' this method only calls the \code{rank_raster} method of a \code{Zresults}
#' object associated with the \code{Zvariant} object. 
#' 
#' @param x \code{\link{Zresults}} or \code{\link{Zvariant}} object.
#'
#' @return \code{\link[raster:RasterLayer-class]{RasterLayer}} object. If no 
#'   results are available, give a warning.
#' 
#' @seealso \code{\link{Zresults-class}}
#' 
#' @export
#' @rdname rank_raster-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("rank_raster", function(x) {
  standardGeneric("rank_raster")
})

# rank_rasters -------------------------------------------------------------

#' Get Zonation result rank rasters of a project.
#' 
#' Each \code{\link{Zproject}} object has a set of variants and their results
#' associated with it. This method will get the selected available rank rasters
#' (1 per variant) and create a \code{\link[raster:RasterStack-class]{RasterStack}} 
#' object. 
#' 
#' Argument \code{variants} can be used to the define which variants are
#' included, the default is to return all. Method will give a warning if a 
#' variant doesn't have a rank raster associated with it. If none of the 
#' variants have a rank raster, then a NA is returned.
#' 
#' @param x \code{\link{Zproject}} object.
#' @param variants a numeric (IDs) or character (name) vector defining which 
#'   variants are included in the returned stack (default: NULL means all).
#'
#' @return \code{\link[raster:RasterStack-class]{RasterStack}} object. If no 
#'   rank rasters are available at all, return NA.
#' 
#' @seealso \code{\link{rank_raster}} \code{\link{get_variant}}
#' 
#' @export
#' @rdname rank_rasters-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("rank_rasters", function(x, variants=NULL) {
  standardGeneric("rank_rasters")
})

# results -----------------------------------------------------------------

#' Getter method for results (\code{Zresults}) in a class 
#' \code{Zvariant} object.
#' 
#' Since not all changes to Zvariant are reflected to its Zresults (e.g. feature
#' and group names) there may quite a lot runtime patching going on.
#' 
#' Results are returned even if only part of them are available. 
#' 
#' @param x Zvariant object.
#'
#' @return Zresults object. If variant doesn't have any results return NA.
#' 
#' @seealso \code{\link{Zresults-class}}
#' 
#' @export
#' @rdname results-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("results", function(x) {
  standardGeneric("results")
})

# set_dat_param -----------------------------------------------------------

#' Set a specified run setting parameter value
#' 
#' Essentially these are parameter values corresponding to Zonation dat-files.
#' This function is used to set values of given parameter. In the current 
#' implementation, sections have no meaning.
#' 
#' @note Method does not check for the sanity of the values provided.
#'
#' @param x Zvariant object.
#' @param parameter Character string name of the parameter.
#' @param value Character string or numeric value of the parameter.
#'
#' @return x Zvariant object.
#' 
#' @seealso \code{\link[zonator:Zvariant-class]{Zvariant-class}} and 
#' \code{\link{get_dat_parameter}}. 
#' 
#' @export
#' @rdname set_dat_param-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("set_dat_param", function(x, parameter, value) {
  standardGeneric("set_dat_param")
})

# sppdata -----------------------------------------------------------------

#' Simple getter mehtod for spp data in a class \code{Zvariant}object.
#' 
#' Method will also return group column with spp data if it exists. 
#' 
#' @param x Zvariant object.
#' @param group.names boolean indicating whether group codes (FALSE) or names
#' (TRUE) are used to indicate group. (default: FALSE)
#'
#' @return Data frame (object's spp.data)
#' 
#' @seealso \code{\link{Zvariant-class}}
#' 
#' @export
#' @rdname sppdata-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("sppdata", function(x, group.names=FALSE) {
  standardGeneric("sppdata")
})

# sppweights --------------------------------------------------------------

#' Get biodiversity feature weights of a Zonation variant
#'
#' @param x Zvariatn object.
#'
#' @return A numeric vector of weights
#' 
#' @seealso \code{\link[zonator:Zvariant-class]{Zvariant-class}}
#' 
#' @export
#' @rdname sppweights-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("sppweights", function(x) {
  standardGeneric("sppweights")
})

# variants ----------------------------------------------------------------

#' Get all variants in a Zonation project
#'
#' @param x Zproject object.
#'
#' @return A list of Zvariant objects
#' 
#' @seealso \code{\link[zonator:Zproject-class]{Zproject-class}} 
#'   and \code{\link[zonator:Zvariant-class]{Zvariant-class}}
#' 
#' @export
#' @rdname variants-methods
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setGeneric("variants", function(x) {
  standardGeneric("variants")
})
