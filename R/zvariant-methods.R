# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomäki <joona.lehtomaki@gmai.com>. All rights 
# reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' @rdname curves-methods
#' @aliases curves,Zresults-method
#' 
setMethod("curves", c("Zvariant"), function(x, cols=NULL, groups=FALSE,
                                            lost.lower=0.0, lost.upper=1.0) {
 return(curves(results(x), cols, groups, lost.lower, lost.upper))
})

#' @name featurenames<-
#' @rdname featurenames-methods
#' @aliases featurenames<-,Zvariant,character-method
#' 
setReplaceMethod("featurenames", c("Zvariant", "character"), function(x, value) {
  # Check names
  value <- check_names(value)
  # Control for length, no cycling allowed
  if (length(value) != nrow(x@spp.data)) {
    stop(paste0("Character vector length (", length(value), " and object spp ",
                "data length (", nrow(x@spp.data), " should be the same"))
  }
  x@spp.data$name <- value
  # Also deal with the resuts data if available
  if (has_results(x)$curves) {
    results.feat.names <- names(x@results@curves)[8:length(x@results@curves)]
    if (length(value) != length(results.feat.names)) {
      stop(paste0("Character vector length (", length(value), " and object results ",
                  "curves header length (", length(results.feat.names), 
                  " should be the same"))
    }
    new.names <- c(names(x@results@curves)[1:7], value)
    names(x@results@curves) <- new.names
  } 
  return(x)
})

#' @rdname featurenames-methods
#' @aliases featurenames,Zvariant-method
#' 
setMethod("featurenames", signature("Zvariant"), function(x) {
  
  if (is.na(x@spp.data) || !"name" %in% names(x@spp.data)) {
    stop("No spp data found or it doesn't have 'name' columnd defined")
  }
  return(x@spp.data$name)
})

#' @rdname groups-methods
#' @aliases groups,Zvariant-method
#' 
setMethod("groups", "Zvariant", function(x) {
  if (any(dim(x@groups) != c(0, 0))) {
    return(x@groups$output.group)
  } else {
    return(NA)
  }
})

#' @rdname groupnames-methods
#' @aliases groupnames,Zvariant-method
#' 
setMethod("groupnames", "Zvariant", function(x) {
  
  if (any(dim(x@groups) == c(0, 0))) {
    return(NA)
  }
  
  # Get all the groups data
  groups.data <- x@groups
  # Get unique codes
  groups.codes <- unique(groups.data$output.group)
  groups.names <- sapply(groups.codes, function(y) {groups.data[which(groups.data$output.group == y),]$name[1]})
  return(groups.names)
})

#' @name groupnames<-
#' @rdname groupnames-methods
#' @aliases groupnames<-,Zvariant,character-method
#' 
setReplaceMethod("groupnames", c("Zvariant", "character"), function(x, value) {
  if (empty(x@groups)) {
    stop("Variant has no groups to name")
  }
  # Actual coded values are vector names. Assume numeric and try to coerce.
  keys <- as.numeric(names(value))
  group.codes <- x@groups$output.group
  # Check that the keys actually are found in codes
  if (!all(keys %in% unique(group.codes))) {
    stop(paste("Key(s)", paste(keys[!keys %in% unique(group.codes)], collapse=", "), 
                               "not found in group codes:", 
               paste(group.codes, collapse=", ")))
  }
  # Get the actual character vector indexes based on the names
  inds <- sapply(group.codes, function(y) {which(keys == y)})
  # Index the value vector
  x@groups$name <- value[inds]
  if (has_results(x)$grp.curves) {
    results.grp.names <- names(x@results@grp.curves)[3:length(x@results@grp.curves)]
    
    # Each group has 5 columns
    ngroups <- length(results.grp.names) / 5
    
    if (length(value) != ngroups) {
      stop(paste0("Character vector length (", length(value), " and object results ",
                  "group curves header length (", ngroups, 
                  " should be the same"))
    }
    for (i in 1:ngroups) {
      group.id <- names(value[i])
      group.name  <- value[[i]]
      results.grp.names <- gsub(paste0("g", group.id), group.name, 
                                results.grp.names)
    }
    new.grp.names <- c(names(x@results@grp.curves)[1:2], results.grp.names)
    names(x@results@grp.curves) <- new.grp.names
  } 
  return(x)
})

#' @rdname has_results-methods
#' 
setMethod("has_results", "Zvariant", function(x) {
  return(has_results(x@results))
})

#' @rdname outdir-methods
#' @aliases outdir,Zvariant-method
#' 
setMethod("outdir", c("Zvariant"), function(x) {
  return(x@output.dir)
})

#' @rdname rank_raster-methods
#' @aliases rank_raster,Zvariant-method
#' 
setMethod("rank_raster", c("Zvariant"), function(x) {
  if(has_results(x)$rank) {
    return(rank_raster(results(x)))
  } else {
    warning("Rank raster requested but not present in ", outdir(x))
  }
})

#' @rdname results-methods
#' @aliases results,Zvariant-method
#' 
setMethod("results", c("Zvariant"), function(x) {
  res <-  unlist(has_results(x))
  # Return results whenever there is at least 1 result item available
  if (any(res)) {
    return(x@results)
  } else {
    warning("Variant doesn't have results")
    return(NA)
  }
})

#' @rdname sppdata-methods
#' @aliases sppdata,Zvariant-method
#' 
setMethod("sppdata", c("Zvariant"), function(x, group.names=FALSE) {
  spp.data <- x@spp.data
  if (!empty(x@groups)) {
    spp.names <- names(spp.data)
    if (group.names == TRUE && "name" %in% names(x@groups)) {
      spp.data <- cbind(spp.data, x@groups$name)
      names(spp.data) <- c(spp.names, "group.name")
    } else {
      spp.data <- cbind(spp.data, x@groups$output.group)
      names(spp.data) <- c(spp.names, "group")
    }
  }
  return(spp.data)
})
