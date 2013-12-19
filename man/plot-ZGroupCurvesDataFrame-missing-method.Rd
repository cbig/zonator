\docType{methods}
\name{plot,ZGroupCurvesDataFrame,missing-method}
\alias{plot,ZGroupCurvesDataFrame,missing-method}
\title{Plot Zonation grouped performance curves.}
\usage{
\S4method{plot}{ZGroupCurvesDataFrame,missing}(x, min = FALSE, mean = FALSE,
  w.mean = FALSE, max = FALSE, ext = FALSE, subs = NULL,
  monochrome = FALSE, invert.x = FALSE, main = "", ...)
}
\arguments{
  \item{x}{data frame containing Zonation's performance
  curve (group-specific) output.}

  \item{statistic}{character string indicating which
  statistic (\code{min}, \code{mean}, \code{max},
  \code{w.mean}, \code{ext2}) over all feature groups is
  plotted.}

  \item{groups}{integer vector containing the IDs of groups
  to be plotted.}

  \item{monochrome}{Boolean indicating if the plot should
  be in monochrome colors only.}

  \item{main}{Character string plot title.}

  \item{invert.x}{Boolean indicating if the X-axis is
  printed from 1 ("feature remaining", \code{FALSE}) or 0
  ("landscape under protection", \code{TRUE}).}

  \item{labels}{character vector for custom feature
  labels.}

  \item{...}{Additional arguments passed on to
  \code{\link{plot}}.}
}
\description{
Plot Zonation grouped performance curves.
}
\author{
Joona Lehtomaki \email{joona.lehtomaki@gmail.com}
}
\seealso{
\code{\link{read_curves}} and \code{\link{plot_curves}}.
}

