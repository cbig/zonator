# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomäki <joona.lehtomaki@gmai.com>. All rights 
# reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

plot.z.curves <- function(x, statistic=NULL, features=NULL, monochrome=FALSE, 
                          invert.x=FALSE, labels=NULL,  ...) {
  
  #browser()
  
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
    col.ids <- col.ids[features]
  }
  col.ids <- c(1, index, col.ids)
  
  # Subset only the needed columns
  x <- x[col.ids]
  
  # List -> DataFrame
  x <- as.data.frame(x)
  
  # Reshape the DataFrame
  x.melt <- melt(data = x, id.vars=c(1), measure.vars=2:length(col.ids))
  
  # Create the necessary widths
  #color.scale <- c(red, gray.colors(length(col.ids) - 2))
  size.scale <- c(2, rep(1, length(col.ids) - 2))
  
  p <- ggplot(x.melt, aes(x=Prop_landscape_lost, y=value, group=variable))
  p <- p + geom_line(aes(colour = variable), size=1.5)
  
  if (monochrome) {
    p <- p + theme_bw() + scale_colour_grey(name=curve.legend.title)
    
  } else {
    p <- p + scale_color_discrete(name=curve.legend.title)
  }
  
  if (invert.x) {
    x.scale <- seq(0, 1, 0.25)
    p + xlab(curve.x.title.invert) + ylab(curve.y.title) + .options["curve.theme"] +
      scale_x_continuous(breaks=x.scale, labels=1-x.scale)
  } else {
    p + xlab(curve.x.title) + ylab(curve.y.title) + .options["curve.theme"]
  }
}

plot.z.grp.curves <- function(x, statistic="mean", groups=NULL, 
                              monochrome=FALSE, invert.x=FALSE, 
                              labels=NULL, ...) {
  
  # Set the statistics indeces
  index <- list("min"=3, "mean"=4, "max"=5, "w.mean"=6, "ext2"=7)
  
  if (statistic %in% names(index)) {
    # Starting from nth column, every 5th column is the same statistic
    col.ids <- seq(index[[statistic]], length(x), 5)
    # Keep also F.lost
    x <- x[c(1, col.ids)]
  } else {
    stop(paste("Unkown statistic type:", statistic))
  }
  
  # Which groups are actually included
  grps <- 2:ncol(x)
  if (!is.null(groups)) {
    grps <- grps[groups]
  }
  
  # Reshape the DataFrame
  x.melt <- melt(data = x, id.vars=c(1), measure.vars=grps)
  
  #default_theme <- theme_get()
  theme_set(theme_bw(22, "Droid sans"))
  
  p <- ggplot(x.melt, aes(x=value, y=F.lost, group=variable))
  p <- p + geom_line(aes(colour = variable))
  
  
  if (is.null(labels)) {
    if (monochrome) {
      p <- p + scale_colour_grey(name=grp.curve.legend.title) + theme_bw() 
    } else {
      p <- p + scale_colour_brewer(name=grp.curve.legend.title) 
    }
  } else {
    if (monochrome) {
      
      p <- p + scale_colour_grey(start=0.0, end=0.8,
                                 name=grp.curve.legend.title, labels=labels) 
        
    } else {
      p <- p + scale_colour_brewer(name=grp.curve.legend.title, labels=labels,
                                   type = "qual", palette=2)
    }
  }
  
  if (invert.x) {
    x.scale <- seq(0, 1, 0.25)
    p <- p + scale_x_continuous(breaks=x.scale, labels=1-x.scale)
  } else {
    
  }
  p + xlab(curve.x.title) + ylab(curve.y.title) + #.options["curve.theme"] + 
    ggtitle(statistic)
  
  # Reset the default theme
  #theme_set(default_theme)
}