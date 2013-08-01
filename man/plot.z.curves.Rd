\name{plot.z.curves}
\alias{plot.z.curves}
\title{Plot Zonation performance curves.}
\usage{
  plot.z.curves(x, statistic = NULL, features = NULL,
    monochrome = FALSE, invert.x = FALSE, labels = NULL,
    ...)
}
\arguments{
  \item{\code{x}}{data frame containing Zonation's
  performance curve (feature-specific) output.}

  \item{\code{statistic}}{character string indicating which
  statistic (\code{min}, \code{mean}) over all features is
  plotted.}

  \item{\code{features}}{integer vector containing the IDs
  of features to be plotted.}

  \item{\code{monochrome}}{Boolean indicating if the plot
  should be in monochrome colors only.}

  \item{\code{invert.x}}{Boolean indicating if the X-axis
  is printed from 1 ("feature remaining", \code{FALSE}) or
  0 ("landscape under protection", \code{TRUE}).}

  \item{\code{labels}}{character vector for custom feature
  labels.}

  \item{\code{...}}{Additional arguments passed on to
  \code{\link{plot}}.}
}
\description{
  Plot Zonation performance curves.
}
\author{
  Joona Lehtomaki \email{joona.lehtomaki@gmail.com}
}
\seealso{
  \code{\link{read.curves}} and
  \code{\link{plot.z.grp.curves}}
}

