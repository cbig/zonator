\docType{methods}
\name{has_results}
\alias{has_results}
\alias{plot}
\alias{plot,Zvariant,missing-method}
\title{Check if an instance of (class \code{Zvariant}) has results.}
\arguments{
  \item{x}{Zvariant object.}

  \item{x}{Zvariant object.}

  \item{group}{boolean indicating whether to plot grouped
  curves.}

  \item{...}{Additional arguments passed on to the speficic
  plotting functions.}
}
\value{
  boolean value

  Zvariant object
}
\description{
  If the results are availbale (i.e. variants have been
  run) then the variant should have a list object
  containing the results.

  In the current implementation, only performance curves
  (feature-specific or grouped) are plotted. In the future,
  plot method should handle other types of results as well.
}
\details{
  Method itself is only a thin wrapper to functions
  \code{\link{curve.plot}} and
  \code{\link{curve.group.plot}}.
}
\author{
  Joona Lehtomaki \email{joona.lehtomaki@gmail.com}

  Joona Lehtomaki \email{joona.lehtomaki@gmail.com}
}
\seealso{
  \code{\link{Zvariant-class}}

  \code{\link{Zvariant-class}}
}

