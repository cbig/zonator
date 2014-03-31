\name{cross_jaccard}
\alias{cross_jaccard}
\title{Calculate Jaccard coefficients bewteen all the RasterLayers within a single
RasterStack.}
\usage{
cross_jaccard(stack, thresholds, ...)
}
\arguments{
  \item{stack}{RasterStack-object.}

  \item{thresholds}{Numeric vector values of thresholds.}

  \item{...}{additional arguments passed on to
  \code{\link{jaccard}}.}
}
\value{
Dataframe with Jaccard coefficients between all the
RasterLayers.
}
\description{
This method is a utility method that is intented to be used
to compare top-fractions of the landscape. Thus, x.max and
y.max for \code{\link{jaccard}} are fixed to 1.0.
}
\author{
Joona Lehtomaki <joona.lehtomaki@gmail.com>
}
\seealso{
\code{\link{jaccard}}
}
\keyword{post-processnig,}
\keyword{ppa}

