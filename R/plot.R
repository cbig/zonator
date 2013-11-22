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
#' @references a \code{ggplot} object containing the plot.
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
#' @param statistic character string indicating which statistic 
#'   (\code{min}, \code{mean}) over all features is plotted.
#' @param features integer vector containing the IDs of features to be
#'   plotted.
#' @param monochrome Boolean indicating if the plot should be in 
#'   monochrome colors only.
#' @param invert.x Boolean indicating if the X-axis is printed from 
#'   1 ("feature remaining", \code{FALSE}) or 0 
#'   ("landscape under protection", \code{TRUE}).  
#' @param labels character vector for custom feature labels.
#' @param ... Additional arguments passed on to \code{\link{plot}}.
#' 
#' @seealso \code{\link{read_curves}} and \code{\link{plot_grp_curves}}
#' 
#' @export
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
#' @examples \dontrun{
#'   # This example assumes that variant do_abf.bat has been run
#'   bat.file <- system.file("extdata/zonation-tutorial", "do_abf.bat",
#'                            package="zonator")
#'   abf.variant <- new("Zvariant", bat.file=bat.file)
#'   plot(abf.variant)
#'   plot(abf.variant, statistic="mean")
#' }
plot_curves <- function(x, statistic=NULL, features=NULL, monochrome=FALSE, 
                          invert.x=FALSE, labels=NULL,  ...) {  
  if (is.null(statistic)) {
    index <- NULL
  } else if (statistic == "min") {
    index <- 3
  } else if (statistic == "mean") {
    index <- 4
  }
  
  col.ids <- 8:length(x)
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
    p + xlab(.options[["curve.x.title"]]) + 
      ylab(.options[["curve.y.title"]])
  }
}

#' Plot Zonation grouped performance curves.
#'
#' @param x data frame containing Zonation's performance curve
#'   (group-specific) output.
#' @param statistic character string indicating which statistic 
#'   (\code{min}, \code{mean}, \code{max}, \code{w.mean}, \code{ext2}) over all 
#'   feature groups is plotted.
#' @param groups integer vector containing the IDs of groups to be
#'   plotted.
#' @param monochrome Boolean indicating if the plot should be in 
#'   monochrome colors only.
#' @param main Character string plot title. 
#' @param invert.x Boolean indicating if the X-axis is printed from 
#'   1 ("feature remaining", \code{FALSE}) or 0 
#'   ("landscape under protection", \code{TRUE}).  
#' @param labels character vector for custom feature labels.
#' @param ... Additional arguments passed on to \code{\link{plot}}.
#' 
#' @seealso \code{\link{read_curves}} and \code{\link{plot_curves}}.
#' 
#' @export
#' 
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#' 
plot_grp_curves <- function(x, statistic="mean", groups=NULL, monochrome=FALSE, 
                            main=NULL, invert.x=FALSE,labels=NULL, ...) {
  
  # Set the statistics indeces
  index <- list("min"=3, "mean"=4, "max"=5, "w.mean"=6, "ext2"=7)
  
  # If no main title is provided, use the statistic provided
  if (is.null(main)) {
    title <- statistic
  } else {
    title <- main
  }
  
  if (statistic %in% names(index)) {
    # Starting from nth column, every 5th column is the same statistic
    col.ids <- seq(index[[statistic]], length(x), 5)
    if (!is.null(groups)) {
      col.ids <- col.ids[groups]
    }
    # Keep also F.lost
    x <- x[c(1, col.ids)]
  } else {
    stop(paste("Unkown statistic type:", statistic))
  }
  
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
  
  # Reshape the DataFrame
  x.melt <- melt(data = x, id.vars=c(1), measure.vars=2:length(x))
  
  #default_theme <- theme_get()
  theme_set(theme_bw(16))
  
  p <- ggplot(x.melt, aes(x=F.lost, y=value, col = variable))
  if (monochrome) {
    p <- p + geom_line(aes(col = variable, linetype = variable), size=1.5)
  } else {
    p <- p + geom_line(size=1.0)
  }
  
  if (monochrome) {
    p <- p + scale_linetype_manual(values = 1:nitems, labels=labels,
                                   name=.options[["grp.curve.legend.title"]]) +
      scale_colour_manual(values=grey.colors(nitems), labels=labels,
                          name=.options[["grp.curve.legend.title"]]) + 
      theme_bw() 
  } else {
    p <- p + scale_colour_brewer(name=.options[["grp.curve.legend.title"]], 
                                 labels=labels, type = "qual", palette=2) 
  }
  
  if (invert.x) {
    x.scale <- seq(0, 1, 0.20)
    y.scale <- seq(0, 1, 0.20)
    p <- p + scale_x_continuous(breaks=x.scale, labels=1-x.scale) + 
      xlab(.options[["curve.x.title.invert"]])
  } else {
    p <- p + xlab(.options[["curve.x.title"]])
  }
  p + ylab(.options[["curve.y.title"]]) + ggtitle(title)
}