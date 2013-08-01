\name{selection.coverage}
\alias{selection.coverage}
\title{Intersection of two coverages.}
\usage{
  selection.coverage(x, y, thresholds)
}
\arguments{
  \item{x}{numeric matrix}

  \item{y}{numeric matrix}

  \item{thresholds}{numeric vector of thresholds used}
}
\value{
  A list with 2 items: \item{thresholds}{Correlations
  between 2 matrices with values above a given threshold.}
  \item{total}{Overall correlation between the 2 matrices.}
}
\description{
  Calculate how much two coverages (as defined by values
  greater than a given threshold in two numeric matrices)
  overlap.
}
\author{
  Joona Lehtomaki <joona.lehtomaki@gmail.com>
}
\keyword{post-processnig,}
\keyword{ppa}

