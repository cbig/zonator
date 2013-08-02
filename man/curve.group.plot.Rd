\name{curve.group.plot}
\alias{curve.group.plot}
\title{Plot Zonation grouped performance curves.}
\usage{
  curve.group.plot(x, statistic = "mean", groups = NULL,
    monochrome = FALSE, main = NULL, invert.x = FALSE,
    labels = NULL, ...)
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
  \code{\link{read.curves}} and \code{\link{curve.plot}}.
}

