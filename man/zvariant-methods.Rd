\docType{methods}
\name{plot}
\alias{plot}
\alias{plot,Zvariant,boolean,ANY-method}
\alias{plot,Zvariant,logical-method}
\title{Plot Zonation variant's (class \code{Zvariant}) results (performance curves).}
\arguments{
  \item{x}{Zvariant object.}

  \item{group}{boolean indicating whether to plot grouped
  curves.}

  \item{...}{Additional arguments passed on to the speficic
  plotting functions.}
}
\value{
  Zvariant object
}
\description{
  In the current implementation, only performance curves
  (feature-specific or grouped) are plotted. In the future,
  plot method should handle other types of results as well.
}
\details{
  Method itself is only a thin wrapper to functions
  \code{\link{plot.z.curves}} and
  \code{\link{plot.z.grp.curves}}.
}
\author{
  Joona Lehtomaki \email{joona.lehtomaki@gmail.com}
}
\seealso{
  \code{\link{Zvariant-class}}
}

