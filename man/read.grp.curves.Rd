\name{read.grp.curves}
\alias{read.grp.curves}
\title{Read in performance curves for grouped features produced by Zonation.}
\usage{
  read.grp.curves(infile)
}
\arguments{
  \item{infile}{Character file path to .curves.txt file.}
}
\value{
  A DataFrame with all the information in the curves file.
  If the requested file does not exist, return NA.
}
\description{
  Header is automatically generated based on the number of
  groups in the file. If you need to read in individual
  curves files, use \code{\link{read.curves}} instead.
}
\author{
  Joona Lehtomaki <joona.lehtomaki@gmail.com>
}
\seealso{
  \code{\link{read.curves}}
}
\keyword{results}
\keyword{zonation,}

