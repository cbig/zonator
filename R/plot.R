# This file is a part of zonator package

# Copyright (C) 2012-2014 Joona Lehtomäki <joona.lehtomaki@gmai.com>. All rights 
# reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

histPlot <- function(x, mask.obj=NULL, add.mean=FALSE, add.median=FALSE, 
                     show=TRUE, save.dir="", binwidth=0.05) {
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
  
  if (!is.null(mask.obj)) {
    name.body <- paste(slot(x, "title"), "_", slot(mask.obj, "title"), sep="")
  } else {
    name.body <- slot(x, "title")
  }
  
  raster.values <- values(raster.obj)
  temp.df <- data.frame(data=raster.values)
  m <- ggplot(temp.df, aes(x = data)) + geom_histogram(colour = "white", 
                                                       binwidth=binwidth) + 
    ggtitle(name.body)
  
  if (add.median) {
    m <- m + geom_vline(xintercept = median(raster.values, na.rm=T), 
                        colour = "red") 
  }
  
  if (add.mean) {
    m <- m + geom_vline(xintercept = mean(raster.values, na.rm=T), 
                        colour = "blue") 
  }
  
  if (show) {
    show(m)
  }
  if (save.dir != "") {
    file.path <- file.path(save.dir, paste("hist_", name.body, ".png", sep=""))
    print(paste("Saving plot to:", file.path))
    ggsave(filename = file.path, plot = m)
  }
}

# Plotting ----------------------------------------------------------------

plot.z.comp.plot <- function(x, y, show=TRUE, ...) {
  
  xrange <- seq(0.1, 1, .1)
  yrange <- seq(0.1, 1, .1)
  #browser()
  # Data structure of x:
  # list
  #  -comparison (list)
  #		-thresh (data frame)
  #		-total	(num)
  
  # Correlations
  windows()
  plot(xrange, yrange, type="n",  xlab="Features alone",
       ylab="Correlation", ylim=c(-0.1, 1.0))
  
  abline(h=0, col="grey")
  comparisons <- length(x)
  colors <- rainbow(comparisons)
  linetype <- c(1:comparisons)
  
  for (i in 1:comparisons){
    data <- x[[i]]$thresh
    #browser()
    lines(xrange, data$correlation, type="l", lwd=1.5,
          lty=linetype[i], col=colors[i])
  }
  legend("topright", legend=names(x), col=colors,
         lty = linetype)
  savePlot("comparisons_correlation.png", type="png")
  if (!show) {
    dev.off()
  }
  
  #browser()
  windows()
  # Coverages
  plot(xrange, yrange, type="n",  xlab="Features alone",
       ylab="Coverage proportion" )
  abline(h=1, col="grey")
  for (i in 1:comparisons){
    data <- x[[i]]$thresh
    #browser()
    lines(xrange, data$cover, type="l", lwd=1.5,
          lty=linetype[i], col=colors[i])
  }
  legend("bottomleft", legend=names(x), col=colors,
         lty = linetype)
  text(10, 20, "foo")
  savePlot("comparisons_coverage.png", type="png")
  if (!show) {
    dev.off()
  }
}

