\name{plot_curves}
\alias{plot_curves}
\title{Plot Zonation performance curves.}
\usage{
  plot_curves(x, min = FALSE, mean = FALSE, w.mean = FALSE,
    features = NULL, monochrome = FALSE, invert.x = FALSE,
    ...)
}
\arguments{
  \item{x}{data frame containing Zonation's performance
  curve (feature-specific) output.}

  \item{min}{logical indicating whether minimum over all
  features is plotted}

  \item{mean}{logical indicating whether the mean over all
  features is plotted}

  \item{w.mean}{logical indicating whether the weighted
  mean over all features is plotted}

  \item{features}{integer vector containing the IDs of
  features to be plotted.}

  \item{monochrome}{Boolean indicating if the plot should
  be in monochrome colors only.}

  \item{invert.x}{Boolean indicating if the X-axis is
  printed from 1 ("feature remaining", \code{FALSE}) or 0
  ("landscape under protection", \code{TRUE}).}

  \item{labels}{character vector for custom feature
  labels.}

  \item{...}{Additional arguments passed on to
  \code{\link{plot}}.}
}
\description{
  Plot Zonation performance curves.
}
\author{
  Joona Lehtomaki \email{joona.lehtomaki@gmail.com}
}
\seealso{
  \code{\link{read_curves}} and
  \code{\link{plot_grp_curves}}
}

