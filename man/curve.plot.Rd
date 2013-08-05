\name{curve.plot}
\alias{curve.plot}
\title{Plot Zonation performance curves.}
\usage{
  curve.plot(x, statistic = NULL, features = NULL,
    monochrome = FALSE, invert.x = FALSE, labels = NULL,
    ...)
}
\arguments{
  \item{x}{data frame containing Zonation's performance
  curve (feature-specific) output.}

  \item{statistic}{character string indicating which
  statistic (\code{min}, \code{mean}) over all features is
  plotted.}

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
\examples{
\dontrun{
  # This example assumes that variant do_abf.bat has been run
  bat.file <- system.file("extdata/zonation-tutorial", "do_abf.bat",
                           package="zonator")
  abf.variant <- new("Zvariant", bat.file=bat.file)
  plot(abf.variant)
  plot(abf.variant, statistic="mean")
}
}
\author{
  Joona Lehtomaki \email{joona.lehtomaki@gmail.com}
}
\seealso{
  \code{\link{read.curves}} and
  \code{\link{curve.group.plot}}
}

