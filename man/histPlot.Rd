\name{histPlot}
\alias{histPlot}
\title{Create a ggplot2 histogram of a \code{RasterLayer}.}
\usage{
  histPlot(x, mask.obj = NULL, add.mean = FALSE,
    add.median = FALSE, save.dir = "", binwidth = 0.05,
    title = NULL)
}
\arguments{
  \item{\code{x}}{\code(RasterLayer) object containing the
  spatial data.}

  \item{\code{mask.obj}}{\code(RasterLayer) object
  optionally used for masking only specific parts of
  \code{x}.}

  \item{\code{add.mean}}{Boolean whether a vertical blue
  line is added to the plot indicating the mean value of
  \code{x}.}

  \item{\code{add.mean}}{Boolean whether a vertical red
  line is added to the plot indicating the median value of
  \code{x}.}

  \item{\code{save.dir}}{Character path (folder) for saving
  the plot as an image.}

  \item{\code{binwidth}}{Double value of bindwidth for
  \code{\link{geom_bar}}.}

  \item{\code{title}}{Character string title of the plot.}
}
\description{
  Create a ggplot2 histogram of a \code{RasterLayer}.
}
\author{
  Joona Lehtomaki \email{joona.lehtomaki@gmail.com}
}
\references{
  a \code{ggplot} object containing the plot.
}
\seealso{
  \code{\link{geom_bar}}.
}

