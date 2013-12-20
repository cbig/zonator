\name{plot_curves}
\alias{plot_curves}
\title{Plot Zonation performance curves.}
\usage{
plot_curves(dat, monochrome = FALSE, invert.x = FALSE,
  main = "Performance curves", legend.title = "Performance",
  groups = FALSE)

plot_curves(dat, monochrome = FALSE, invert.x = FALSE,
  main = "Performance curves", legend.title = "Performance",
  groups = FALSE)
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

  \item{...}{Additional arguments passed on to
  \code{\link{plot}}.}

  \item{x}{data frame of curves of groups curves data.}

  \item{monochrome}{logical defining if a monochrome color
  scheme is used (default: FALSE).}

  \item{invert.x}{logical indicating if the X-axis is
  printed from 1 ("feature remaining", \code{FALSE}) or 0
  ("landscape under protection", \code{TRUE}).}

  \item{main}{character string title for the plot
  (deafault: 'Performance curves').}

  \item{legend.title}{character string title for legend
  (default: 'Performance) .}

  \item{groups}{logical indicating whether the plotted data
  is groups or not (default: FALSE).}
}
\value{
ggplot2 object
}
\description{
Plot Zonation performance curves.

Method ...
}
\author{
Joona Lehtomaki \email{joona.lehtomaki@gmail.com}

Joona Lehtomaki \email{joona.lehtomaki@gmail.com}
}
\seealso{
\code{\link{read_curves}} and \code{\link{plot_grp_curves}}
}
\keyword{internal}

