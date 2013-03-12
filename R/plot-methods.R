plot.z.curves <- function(x, statistic=NULL, features=NULL, monochrome=FALSE, 
                          invert.x=FALSE, ...) {
  
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
                              monochrome=FALSE, invert.x=FALSE, ...) {
  
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
  
  p <- ggplot(x.melt, aes(x=value, y=F.lost, group=variable))
  p <- p + geom_line(aes(colour = variable), size=1.5)
  
  if (monochrome) {
    p <- p + theme_bw() + scale_colour_grey(name=grp.curve.legend.title)
    
  } else {
    p <- p + scale_color_discrete(name=grp.curve.legend.title)
  }
  
  if (invert.x) {
    x.scale <- seq(0, 1, 0.25)
    p + xlab(curve.x.title.invert) + ylab(curve.y.title) + .options["curve.theme"] +
      scale_x_continuous(breaks=x.scale, labels=1-x.scale)
  } else {
    p + xlab(curve.x.title) + ylab(curve.y.title) + .options["curve.theme"]
  }
  
}