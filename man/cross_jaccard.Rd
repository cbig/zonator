\name{cross_jaccard}
\alias{cross_jaccard}
\title{Calculate Jaccard coefficients bewteen all the RasterLayers within a single
RasterStack.}
\usage{
cross_jaccard(stack, threshold)
}
\arguments{
  \item{stack}{RasterStack-object.}

  \item{threshold}{Numeric value of threshold.}
}
\value{
A matrix with Jaccard coefficients between all the
RasterLayers.
}
\description{
Calculate Jaccard coefficients bewteen all the RasterLayers
within a single RasterStack.
}
\author{
Joona Lehtomaki <joona.lehtomaki@gmail.com>
}
\seealso{
\code{\link{jaccard}}
}
\keyword{post-processnig,}
\keyword{ppa}

