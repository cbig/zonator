 # This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomaki <joona.lehtomaki@gmai.com>. All rights
# reserved.

# This program is open source software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' @rdname get_variant-methods
#' @aliases get_variant,Zproject-method
#'
setMethod("get_variant", c("Zproject", "ANY"), function(x, index) {
  # Check the index
  if (is.numeric(index)) {
    if (!index %in% 1:nvariants(x)) {
      stop(paste("Numeric index", index, "not valid"))
    }
  } else if (is.character(index)) {
    if (!index %in% names(x@variants)) {
      stop(paste("Character index", index, "not valid"))
    }
  }
  return(x@variants[[index]])
})

#' @rdname nvariants-methods
#' @aliases nvariants,Zproject-method
#'
setMethod("nvariants", "Zproject", function(x) {
  return(length(x@variants))
})

#' Names of variants in Zproject
#'
#' Get the names of all the variants within a given \code{\link{Zproject}}.
#'
#' @param x \code{Zproject} object.
#'
#' @rdname names-methods
#' @aliases names,Zproject-method
#'
setMethod("names", "Zproject", function(x) {
  return(names(x@variants))
})

#' @rdname opendir-methods
#' @aliases opendir,Zproject-method
#'
setMethod("opendir", "Zproject", function(x) {
  if (.Platform['OS.type'] == "windows"){
    shell.exec(x@root)
  } else {
    system(paste("dolphin", x@root, "&"))
  }
})

#' @rdname rank_rasters-methods
#' @aliases rank_rasters,Zproject-method
#'
setMethod("rank_rasters", c("Zproject"), function(x, variants=NULL) {
  # Place the rank rasters into a list
  rank_rasters_list <- list()

  # Check if only a subset of variants is needed. If not, get all the variants
  # based on their ID.
  if (is.null(variants)) {
    variants <- 1:nvariants(x)
  }
  # Loop over the variant IDs
  for (variant.id in variants) {

    variant <- get_variant(x, variant.id)
    if(has_results(variant)$rank) {
      rank_rasters_list[variant@name] <- rank_raster(variant)
    } else {
      warning("Variant with ID ", variant.id, " does not have a rank raster")
    }
  }
  if (length(rank_rasters_list) == 0) {
    warning("None of the variants have rank rasters")
    return(NA)
  } else {
    return(raster::stack(rank_rasters_list))
  }
})

#' @rdname variants-methods
#' @aliases variants,Zproject-method
#'
setMethod("variants", "Zproject", function(x) {
  return(x@variants)
})
