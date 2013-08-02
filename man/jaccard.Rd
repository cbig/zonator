\name{jaccard}
\alias{jaccard}
\title{Calculate the Jaccard coefficient.}
\usage{
  jaccard(x, y, threshold, warn.uneven = FALSE)
}
\arguments{
  \item{x}{raster object.}

  \item{y}{raster object.}

  \item{threshold}{Numeric value of threshold.}

  \item{warn.uneven}{Logical indicating whether a warning
  is raised if the compared raster coverages are very
  (>20x) uneven.}
}
\value{
  A numeric value [0, 1]
}
\description{
  The Jaccard coefficient measures similarity between
  sample sets, and is defined as the size of the
  intersection divided by the size of the union of the
  sample sets. The Jaccard coefficient can be calculated
  for a subset of rasters provided by using the threshold
  argument.
}
\author{
  Joona Lehtomaki <joona.lehtomaki@gmail.com>
}
\keyword{post-processnig,}
\keyword{ppa}

