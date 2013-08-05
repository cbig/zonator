\name{read.curves}
\alias{read.curves}
\title{Read in performance curves produced by Zonation.}
\usage{
  read.curves(infile)
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
  features in the file. If you need to read in grouped
  curves files, use \code{\link{read.grp.curves}} instead.
}
\author{
  Joona Lehtomaki <joona.lehtomaki@gmail.com>
}
\seealso{
  \code{\link{read.grp.curves}}
}
\keyword{results}
\keyword{zonation,}

