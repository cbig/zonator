\name{plot.z.grp.curves}
\alias{plot.z.grp.curves}
\title{Plot Zonation grouped performance curves.}
\usage{
  plot.z.grp.curves(x, statistic = "mean", groups = NULL,
    monochrome = FALSE, main = NULL, invert.x = FALSE,
    labels = NULL, ...)
}
\arguments{
  \item{\code{x}}{data frame containing Zonation's
  performance curve (group-specific) output.}

  \item{\code{statistic}}{character string indicating which
  statistic (\code{min}, \code{mean}, \code{max},
  \code{w.mean}, \code{ext2}) over all feature groups is
  plotted.}

  \item{\code{groups}}{integer vector containing the IDs of
  groups to be plotted.}

  \item{\code{monochrome}}{Boolean indicating if the plot
  should be in monochrome colors only.}

  \item{\code{main}}{Character string plot title.}

  \item{\code{invert.x}}{Boolean indicating if the X-axis
  is printed from 1 ("feature remaining", \code{FALSE}) or
  0 ("landscape under protection", \code{TRUE}).}

  \item{\code{labels}}{character vector for custom feature
  labels.}

  \item{\code{...}}{Additional arguments passed on to
  \code{\link{plot}}.}
}
\description{
  Plot Zonation grouped performance curves.
}
\author{
  Joona Lehtomaki \email{joona.lehtomaki@gmail.com}
}
\seealso{
  \code{\link{read.curves}} and \code{\link{plot.z.curves}}
}

