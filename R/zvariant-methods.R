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

#' @rdname featurenames-methods
#' 
setMethod("featurenames", signature("Zvariant"), function(x) {
  
  if (is.na(x@spp.data) || !"name" %in% names(x@spp.data)) {
    stop("No spp data found or it doesn't have 'name' column defined")
  }
  return(x@spp.data$name)
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
  # Also deal with the results data if available
  if (any(dim(x@results@curves) != c(0, 0))) {
    featurenames(x@results@curves) <- value
  }
  return(x)
})

#' @rdname groups-methods
#' 
setMethod("groups", "Zvariant", function(x) {
  if (any(dim(x@groups) != c(0, 0))) {
    return(x@groups$output.group)
  } else {
    return(NA)
  }
})

#' @name groups<-
#' @rdname groups-methods
#' @aliases groups<-,Zvariant,numeric-method
#' 
setReplaceMethod("groups", c("Zvariant", "numeric"), function(x, value) {
  
  # Check that the number of provided group codes matches with the number of
  # features
  nfeats <- nfeatures(x)
  nreplacement <- length(value)
  
  if (nfeats > nreplacement) {
    stop("Too few replacement group IDs (", nreplacement, ") for variant ",
         "features (", nfeats, ")")
  } else if (nfeats < nreplacement) {
    stop("Too many replacement group IDs (", nreplacement, ") for variant ",
         "features (", nfeats, ")")
  }
  
  # If nothing has been assigned, assign values. If nothing changes, do nothing.
  if (is.null(x@groups$output.group) | !all(x@groups$output.group == value)) {
    # In case no groups dataframe has been populated, do that
    if (nrow(x@groups) == 0L) {
      # FIXME: not a good idea to define names(x@groups) in several places...
      x@groups <- data.frame(output.group = value, 
                             condition.group = -1,
                             retention.group = -1,
                             retention.mode = 1, 
                             local.edge.correct.group = -1,
                             name = "")
    } else {
      x@groups$output.group <- value
    }
    # Update results curves data if it exists
    #browser()
    if (has_results(x)[["curves"]]) {
      x@results@grp.curves <- regroup_curves(curves(x), sppweights(x), value)
    }
    # Change group names back to generic "group1", "group2" etc.
    group.ids <- unique(value)
    group.names <- paste0("group", group.ids)
    names(group.names) <- group.ids
    groupnames(x) <- group.names
  }
  
  return(x)
})

#' @rdname groupnames-methods
#' @export
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
  if (plyr::empty(x@groups)) {
    stop("Variant has no groups to name")
  }
  # Actual coded values are vector names. Assume numeric and try to coerce.
  keys <- as.numeric(names(value))
  group.codes <- groups(x)
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
      stop(paste0("Character vector length (", length(value), ") and object ",
                  "results group curves header length (", ngroups, 
                  ") should be the same"))
    }
    for (i in 1:ngroups) {
      group.id <- names(value[i])
      group.name  <- value[[i]]
      results.grp.names <- gsub(paste0("group", group.id, "$"), group.name, 
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

#' @rdname nfeatures-methods
#' @aliases nfeatures,Zvariant-method
#' 
setMethod("nfeatures", "Zvariant", function(x) {
  return(nrow(x@spp.data))
})

#' @rdname outdir-methods
#' 
setMethod("outdir", c("Zvariant"), function(x) {
  return(x@output.dir)
})

#' Print Zvariant information.
#'
#' Generic printing function
#'
#' @param x \code{ZVariant} object.
#'
#' @rdname print-methods
#'
#' @export
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setMethod('print' , c("Zvariant"), function(x) {
  .printZvariant(x)
})	

#' @rdname rank_raster-methods
#' 
setMethod("rank_raster", c("Zvariant"), function(x) {
  if (has_results(x)$rank) {
    return(rank_raster(results(x)))
  } else {
    warning("Rank raster requested but not present in ", outdir(x))
  }
})

#' @rdname results-methods
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

#' Print Zvariant information.
#'
#' Generic printing function
#'
#' @param object \code{ZVariant} object.
#'
#' @rdname show-methods
#'
#' @export
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
setMethod('show' , c("Zvariant"), function(object) {
  .printZvariant(object)
})

#' @rdname sppdata-methods
#' 
setMethod("sppdata", c("Zvariant"), function(x, group.names=FALSE) {
  spp.data <- x@spp.data
  if (!plyr::empty(x@groups)) {
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

#' @rdname sppweights-methods
#' 
setMethod("sppweights", c("Zvariant"), function(x) {
  return(x@spp.data$weight)
})

.printZvariant <- function(x, ...) {
  
  cat('name       :', x@name, '\n')
  cat('bat-file   :', x@bat.file, '\n')
  # Show max first 5 biodiversity feature names
  spp_names <- featurenames(x)
  if (length(spp_names) > 5) {
    spp_names <- paste(c(spp_names[1:5], "..."), collapse = ", ")
  } else {
    spp_names <- paste(c(spp_names), collapse = ", ")
  }
  cat('features   :', nrow(x@spp.data), paste0("[", spp_names, "]"), '\n')
  # Show max first 5 group names
  grp_names <- groupnames(x)
  if (length(grp_names) > 5) {
    grp_names <- paste(c(grp_names[1:5], "..."), collapse = ", ")
  } else {
    grp_names <- paste(c(grp_names), collapse = ", ")
  }
  cat('groups     :', length(unique(groupnames(x))), 
      paste0("[", grp_names, "]"), '\n')
  if (any(unlist(has_results(x)))) {
    res_string <- paste0("yes (created: ", as.character(x@results@modified), 
                         ")") 
  } else {
    res_string <- "no"
  }
  cat("has results:", res_string, '\n')
}