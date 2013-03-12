# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomäki <joona.lehtomaki@gmai.com>. All rights 
# reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

## A function to compare to to matrices in different ways, for example when
## comparing solutions created bu Zonation
## Parameters:
## x - matrix of values
## y - matrix of values
## fun - function that is used for comparison

comp <- function(x, y, fun="correlation", ...) {
  
  # Check the data
  if (!is.matrix(x) | !is.matrix(y)) {
    stop("Both inputs must be matrices.")
  }
  if (dim(x)[1] != dim(y)[1] | dim(x)[2] != dim(y)[2]) {
    stop("Matrix dimensions do not match.")
  }
  
  switch(fun,
         correlation = correlation(x, y, ...),
         substraction = substraction(x, y),
         frequency = selection.frequency(x, y, ...),
         coverage = selection.coverage(x, y, ...))
}

compare.solutions <- function(file1, file2, ...) {
  # Read in the solutions
  sol1 <- hg.read.asc.file(file1, rm.nodata=-1)
  sol2 <- hg.read.asc.file(file2, rm.nodata=-1)
  subs <- comp(sol1, sol2, fun="substraction")
  
  tsh <-  seq(0, 0.9, 0.1)
  corr <- comp(sol1, sol2, fun="correlation", thresholds=tsh)
  cover <- comp(sol1, sol2, fun="coverage", thresholds=tsh)
  return(list(thresholds=cbind(corr$classes, cover), totalcor=corr$total,
              subs=subs))
}

comp.suite <- function(x, input) {
  for (item in x){
    
    res <- compare.solutions(paste(item[1], ".rank.asc", sep=""),
                             paste(item[2], ".rank.asc", sep=""))
    
    filename <- paste(input, "comparisons_", item[1], "_", item[2],
                      ".cmp", sep="")
    
    write.table(res$thresholds, filename, col.names = TRUE, row.names = TRUE,
                quote=FALSE)
    
    cat(file=filename, paste("Total correlation:", res$totalcor, "\n"),
        append=TRUE)
    
    write.asc.file(res$subs, filename, nrow(res$subs),
                   ncol(res$subs))
    
    plot(read.stats(), show=FALSE)
    
  }
}

correlation <- function(x, y, method="spearman", thresholds=c(0)) {
  
  res <- c()
  for (i in 1:length(thresholds)) {
    x.sel <- as.vector(x[which(x > thresholds[i])])
    y.sel <- as.vector(y[which(y > thresholds[i])])
    res <- append(res, cor(x.sel, y.sel, method=method))
  }
  
  res <- data.frame(res, row.names=thresholds)
  colnames(res) <- "correlation"
  # Returns a list [1] threshold class correlations (data frame), [2] total
  # correlation
  return(list(classes=res, total=cor(as.vector(x), as.vector(y),
                                     method=method)))
}

selection.coverage <- function(x, y, thresholds) {
  
  covs <- c()
  total <- c()
  for (thresh in thresholds) {
    sel1 <- which(x >= thresh)
    sel2 <- which(y >= thresh)
    
    # All produce the same indices -> is this real or not?
    total <- append(total, length(sel1) / length(x))
    covs <- append(covs, sum(sel1 %in% sel2) / length(sel1))
  }
  
  res <- data.frame(total=total, cover=covs, row.names=thresholds)
  return(res)
  plot(read.stats(), show=FALSE)
}

substraction <- function(x, y) {
  return(x - y)
}

# From: http://r-sig-geo.2731867.n2.nabble.com/questions-on-RasterStack-Brick-td5553580.html

stackcor <- function(s1, s2, method='spearman') {
  mycor <- function(v) {
    x <- v[1:split]
    y <- v[(split+1):(2*split)]
    cor(x, y, method=method)
  }
  s <- stack(s1, s2)
  split <- nlayers(s)/2
  calc(s, fun=mycor )
}

# The Jaccard coefficient measures similarity between sample sets, and is 
# defined as the size of the intersection divided by the size of the union of 
# the sample sets

jaccard <- function(raster1, raster2, threshhold, warn.uneven=FALSE) {
  
  # Get the values above the threshhold
  raster1.bin <- raster1 > threshhold
  raster2.bin <- raster2 > threshhold
  
  if (warn.uneven) {
    raster1.size <- count(raster1.bin, 1)
    raster2.size <- count(raster2.bin, 1)
    # Sort from smaller to larger
    sizes <- sort(c(raster1.size, raster2.size))
    if (sizes[2] / sizes[1] > 20) {
      warning("The extents of raster values above the threshhold differ more than 20-fold: Jaccard coefficient may not be informative.")
    }
  }
  
  # Calculate the intersection of the two rasters, this is given by adding 
  # the binary rasters together -> 2 indicates intersection
  combination <- raster1.bin + raster2.bin
  intersection <- combination == 2
  
  # Union is all the area covered by the both rasters
  union <- combination >= 1
  
  return(count(intersection, 1) / count(union, 1))
}

cross.jaccard <- function(results, cut.off) {
  
  jaccards <- matrix(nrow=nlayers(results), ncol=nlayers(results))
  
  for (i in 1:nrow(jaccards)) {
    for (j in 1:ncol(jaccards)) {
      if (i == j) {
        jaccards[i, j] <- 1
      } else {
        # See the complement, if it's not NA then the pair has already been
        # compared
        if (is.na(jaccards[j, i])) {
          jaccards[i, j] <- jaccard(results[[i]], results[[2]], cut.off)
        } else {
          jaccards[i, j]  <- jaccards[j, i]
        }
      }
    }
  }
  return(jaccards)
}