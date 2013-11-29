\name{read_curves}
\alias{read_curves}
\title{Read in performance curves produced by Zonation.}
\usage{
  read_curves(infile)
}
\arguments{
  \item{infile}{Character file path to .curves.txt file.}
}
\value{
  Curves object with all the information in the curves
  file. If the requested file does not exist, return NA.
}
\description{
  Header is automatically generated based on the number of
  features in the file. If you need to read in grouped
  curves files, use \code{\link{read_grp_curves}} instead.
}
\author{
  Joona Lehtomaki <joona.lehtomaki@gmail.com>
}
\seealso{
  \code{\link{read_grp_curves}}
}
\keyword{results}
\keyword{zonation,}

