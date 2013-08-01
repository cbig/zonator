\name{correlation}
\alias{correlation}
\title{Correlation between two matrices.}
\usage{
  correlation(x, y, method = "kendall", thresholds = c(0))
}
\arguments{
  \item{x}{numeric matrix}

  \item{y}{numeric matrix}

  \item{method}{String correlation method used (default:
  'kendall')}

  \item{thresholds}{numeric vector of thresholds used
  (default: c(0))}
}
\value{
  A list with 2 items: \item{thresholds}{Correlations
  between 2 matrices with values above a given threshold.}
  \item{total}{Overall correlation between the 2 matrices.}
}
\description{
  Calculate correlation between two matrices using
  \code{\link{cor}}. A group of specific threshold can be
  set, in which case the correlations are calculated
  incrementally for values above the thresholds.
}
\author{
  Joona Lehtomaki <joona.lehtomaki@gmail.com>
}
\seealso{
  \code{\link{cor}}
}
\keyword{post-processnig,}
\keyword{ppa}

