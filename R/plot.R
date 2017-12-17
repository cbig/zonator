# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomaki <joona.lehtomaki@gmai.com>. All rights
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
#' @import ggplot2
#' @importFrom stats median
#' @importFrom utils data
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
  m <- ggplot(temp.df, aes(x = data)) +
    geom_histogram(colour = "white", binwidth=binwidth) +
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

#' Create ggplot2 object for performance curves.
#'
#' Method ...
#'
#' @param x data frame of curves of groups curves data.
#' @param monochrome logical defining if a monochrome color scheme is used
#' (default: FALSE).
#' @param invert.x logical indicating if the X-axis is printed from
#'   1 ("feature remaining", \code{FALSE}) or 0
#'   ("landscape under protection", \code{TRUE}).
#' @param fix.x logical indicating if y-axis should be fixed to [0, 1] (TRUE) or
#'   not (FALSE).
#' @param main character string title for the plot (deafault: 'Performance
#' curves').
#' @param legend.title character string title for legend (default: 'Performance)
#' .
#' @param groups logical indicating whether the plotted data is groups or not
#' (default: FALSE).
#'
#' @return ggplot2 object
#'
#' @import ggplot2
#' @importFrom grDevices grey.colors
#'
#' @keywords internal
#'
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.com}
#'

plot_curves <- function(dat, monochrome=FALSE, invert.x=FALSE, fix.y=FALSE,
                        main='Performance curves', legend.title='Performance',
                        groups=FALSE) {

  if (groups) {
    iact.levels <- levels(interaction(dat$name, dat$stat))
    iact.labels <- gsub('\\.', ', ', iact.levels)
    iact.names <- gsub('\\..*$', '', iact.levels)
    iact.stats <- gsub('^.*\\.', '', iact.levels)
  } else {
    iact.levels <- levels(dat$name)
    iact.labels <- gsub('\\.', ' ', iact.levels)
    iact.names <- iact.levels
    iact.stats <- unique(data.frame(name=dat$name, stat=dat$stat))$stat
  }

  if (groups) {
    lty.map <- list('mean'=1, 'min'=3, 'max'=2, 'w.mean'=4, 'ext2'=5)
    size.map <- list('mean'=1.1, 'min'=0.7, 'max'=0.7, 'w.mean'=0.7, 'ext2'=0.7)
  } else {
    lty.map <- list('feature'=1, 'mean'=2, 'min'=3, 'w.mean'=4, 'ext2'=5)
    size.map <- list('feature'=1.1, 'mean'=0.7, 'min'=0.7, 'w.mean'=0.7,
                     'ext2'=0.7)
  }


  lty.values <- sapply(iact.stats, function(x) {
    lty.map[[grep(paste0('^', x), names(lty.map))]]
    }, USE.NAMES=FALSE)
  size.values <- sapply(iact.stats, function(x) {
    size.map[[grep(paste0('^', x), names(size.map))]]
    }, USE.NAMES=FALSE)

  if (monochrome) {
    cols <- grey.colors(n=length(unique(iact.names)))
  } else {
    nfeatures <- length(unique(iact.names))
    # Decide which color set to use
    colorset <- ifelse(nfeatures <= 9, "Set1", "Set3")
    cols <- suppressWarnings(RColorBrewer::brewer.pal(nfeatures,
                                                      colorset))
  }

  colour.values <- sapply(iact.names, function(x) {
    cols[which(x == unique(iact.names))]
    }, USE.NAMES=FALSE)


  if (groups) {
    dat$name_x_stat <- interaction(dat$name, dat$stat)
    p <- ggplot(dat, aes_(x=~pr_lost, y=~value,
                          colour=~name_x_stat))
    p <- p + geom_line(aes_(linetype=~name_x_stat,
                       size=~name_x_stat))
  } else {
    p <- ggplot(dat, aes_(x=~pr_lost, y=~value, colour=~name))
    p <- p + geom_line(aes_(linetype=~name,
                           size=~name))
  }

  p <- p + scale_colour_manual(name=legend.title,
                               breaks=iact.levels,
                               labels=iact.labels,
                               values=colour.values) +
           scale_linetype_manual(name=legend.title,
                                 breaks=iact.levels,
                                 labels=iact.labels,
                                 values=lty.values) +
           scale_size_manual(name=legend.title,
                             breaks=iact.levels,
                             labels=iact.labels,
                             values=size.values)
  x.scale <- seq(0, 1, 0.2)
  y.scale <- seq(0, 1, 0.2)

  if (invert.x) {
    p <- p + xlab(.options$curve.x.title.invert) +
      ylab(.options$curve.y.title) +
      scale_x_continuous(breaks=x.scale, labels=1-x.scale) +
      scale_y_continuous(breaks=y.scale, labels=y.scale)
  } else {
    p <- p + xlab(.options$curve.x.title) + ylab(.options$curve.y.title)
  }

  p <- p  + ggtitle(main)
  p <- p + .options$curve.theme

  if (fix.y) {
    p <- p + ylim(0, 1)
  }

  return(p)
}
