# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomäki <joona.lehtomaki@gmai.com>. All rights 
# reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' Create a ggplot2 histogram of a \code{RasterLayer}.
#'
#' @param x \code{RasterLayer} object containing the spatial data.
#' @param mask.obj \code{RasterLayer} object optionally used for masking
#'   only specific parts of \code{x}.
#' @param add.mean Boolean whether a vertical blue line is added to the
#'   plot indicating the mean value of \code{x}.
#' @param add.median Boolean whether a vertical red line is added to the
#'   plot indicating the median value of \code{x}.
#' @param save.dir Character path (folder) for saving the plot as an 
#'   image.
#' @param binwidth Double value of bindwidth for \code{\link{geom_bar}}.
#' @param title Character string title of the plot.
#' 
#' @return a \code{ggplot} object containing the plot.
#' 
#' @seealso \code{\link{geom_bar}}.
#' 
#' @export
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
plot_hist <- function(x, mask.obj=NULL, add.mean=FALSE, add.median=FALSE, 
                     save.dir="", binwidth=0.05, title=NULL) {
  if (class(x) != "RasterLayer") {
    stop("x must be an object of class 'RasterLayer'!")
  } 
  if (!is.null(mask.obj)) {
    if (class(mask.obj) == "RasterLayer") {
      raster.obj <- mask(x, mask.obj)
    } else {
      print("Mask provided is not a RasterLayer, skipping")
    }
  } else {
    raster.obj <- x
  }
  
  if (is.null(title)) { 
    if (!is.null(mask.obj)) {
      title <- paste(names(x), "_", names(mask.obj), sep="")
    } else {
      title <- names(x)
    }
  }
    
  raster.values <- values(raster.obj)
  temp.df <- data.frame(data=raster.values)
  m <- ggplot(temp.df, aes(x = data)) + geom_histogram(colour = "white", 
                                                       binwidth=binwidth) + 
    scale_x_continuous(breaks=seq(0, 1, 0.25)) + 
    xlab("Priority rank") + ylab("Count") + ggtitle(title) + theme_bw()
  
  if (add.median) {
    m <- m + geom_vline(xintercept = median(raster.values, na.rm=T), 
                        colour = "red") 
  }
  
  if (add.mean) {
    m <- m + geom_vline(xintercept = mean(raster.values, na.rm=T), 
                        colour = "blue") 
  }

  if (save.dir != "") {
    # FIXME: x@title may not work
    file.path <- file.path(save.dir, paste("hist_", x@title, ".png", sep=""))
    print(paste("Saving plot to:", file.path))
    ggsave(filename = file.path, plot = m)
  }
  return(m)
}

#' Plot Zonation performance curves.
#'
#' @param x data frame containing Zonation's performance curve
#'   (feature-specific) output.
#' @param min logical indicating whether minimum over all features is plotted
#' @param mean logical indicating whether the mean over all features is plotted
#' @param w.mean logical indicating whether the weighted  mean over all features 
#' is plotted
#' @param features integer vector containing the IDs of features to be
#'   plotted.
#' @param monochrome Boolean indicating if the plot should be in 
#'   monochrome colors only.
#' @param invert.x Boolean indicating if the X-axis is printed from 
#'   1 ("feature remaining", \code{FALSE}) or 0 
#'   ("landscape under protection", \code{TRUE}).  
#' @param ... Additional arguments passed on to \code{\link{plot}}.
#' 
#' @seealso \code{\link{read_curves}} and \code{\link{plot_grp_curves}}
#' 
#' @export
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
plot_curves <- function(x, min=FALSE, mean=FALSE, w.mean=FALSE, features=NULL, 
                        monochrome=FALSE, invert.x=FALSE, ...) {  
  
  extras <- NULL
  
  if (min) {
    extras <- c(extras, 3)
  }
  if (mean) {
    extras <- c(extras, 4)
  }
  if (w.mean) {
    extras <- c(extras, 5)
  }

  col.ids <- 8:ncol(x)
  # Which features are actually included
  if (!is.null(features)) {
    # Don't include the 1st columns ave_prop_remp
    col.ids <- col.ids[features]
  }
  col.ids <- c(1, index, col.ids)
  
  # Subset only the needed columns
  x.selected <- x[col.ids]
  
  # Reshape the DataFrame
  x.melt <- melt(data = x.selected, id.vars=c(1), measure.vars=2:length(col.ids))
  x.melt$size  <- ifelse(x.melt$variable == names(x)[index], 2, 1)
  
  # How many items are we drawing (excluding F.lost)
  nitems <- length(col.ids)
  
  # Check that there is the right number of labels if provided
  if (!is.null(labels)) {
    if (nitems != length(labels)) {
      stop(paste0("The number or plot items (", nitems, ") and labels (", 
                  length(labels), ") differs"))
    }
  } else {
    # Tell ggplot to use defaults for the labels
    labels <- waiver()
  }
  
  p <- ggplot(x.melt, aes(x=Prop_landscape_lost, y=value, group=variable))
  p <- p + geom_line(aes(colour = variable), size=1.0)
  
  if (monochrome) {
    p <- p + theme_bw() + scale_colour_grey(name=.options[["curve.legend.title"]])
    
  } else {
    p <- p + scale_colour_brewer(name=.options[["curve.legend.title"]],
                                 palette="Set1", labels=labels)
  }
  
  x.scale <- seq(0, 1, 0.2)
  y.scale <- seq(0, 1, 0.2)
  
  if (invert.x) {
    
    p + xlab(.options[["curve.x.title.invert"]]) + 
      ylab(.options[["curve.y.title"]]) +
      scale_x_continuous(breaks=x.scale, labels=1-x.scale) + 
      scale_y_continuous(breaks=y.scale, labels=y.scale)
  } else {
    p + xlab(.options[["curve.x.title"]]) + ylab(.options[["curve.y.title"]])
  }
  
  p <- p + ylab(.options[["curve.y.title"]]) + ggtitle(title)
  return(p + .options[["curve.theme"]])
}

#' Create ggplot2 object for performance curves.
#' 
#' Method ...
#' 
#' @param x data frame of curves of groups curves data.
#' 
#' @return ggplot2 object
#' 
#' @keywords internal
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 

plot_curves <- function(dat) {
  
  iact.levels <- levels(interaction(dat$group, dat$stat))
  iact.labels <- gsub('\\.', ', ', iact.levels)
  iact.groups <- gsub('\\..*$', '', iact.levels)
  iact.stats <- gsub('^.*\\.', '', iact.levels)
  lty.map <- list('mean'=1, 'min'=3, 'max'=2, 'w.mean'=4, 'ext2'=5)
  size.map <- list('mean'=1.1, 'min'=0.7, 'max'=0.7, 'w.mean'=0.7, 'ext2'=0.7)
  lty.values <- sapply(iact.stats, function(x) {
    lty.map[[grep(paste0('^', x), names(lty.map))]]
    }, USE.NAMES=FALSE)
  size.values <- sapply(iact.stats, function(x) {
    size.map[[grep(paste0('^', x), names(size.map))]]
    }, USE.NAMES=FALSE)
  cols <- suppressWarnings(brewer.pal(length(unique(iact.groups)), "Set1"))
  colour.values <- sapply(iact.groups, function(x) {
    cols[which(x == unique(iact.groups))]
    }, USE.NAMES=FALSE)
  #browser()
  p <- ggplot(dat, aes(x=pr_lost, y=value, colour=interaction(group, stat)))
  p <- p + geom_line(aes(linetype=interaction(group, stat), 
                     size=interaction(group, stat))) + 
        scale_colour_manual(name="Performance",
                            breaks=iact.levels,
                            labels=iact.labels,
                            values=colour.values) +
        scale_linetype_manual(name="Performance",
                              breaks=iact.levels,
                              labels=iact.labels,
                              values=lty.values) +
        scale_size_manual(name="Performance",
                          breaks=iact.levels,
                          labels=iact.labels,
                          values=size.values)
  return(p)
}