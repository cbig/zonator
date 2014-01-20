\docType{methods}
\name{rank_rasters}
\alias{rank_rasters}
\alias{rank_rasters,Zproject-method}
\title{Get all Zonation result rank rasters of a project.}
\usage{
rank_rasters(x)

\S4method{rank_rasters}{Zproject}(x)
}
\arguments{
  \item{x}{\code{\link{Zproject}} object.}
}
\value{
\code{\link{RasterStack}} object.
}
\description{
Each \code{\link{Zproject}} object has a set of variants
and their results associated with it. This method will get
all the available rank rasters (1 per variant) and create a
\code{\link{RasterStack}} object.
}
\author{
Joona Lehtomaki \email{joona.lehtomaki@gmail.com}
}
\seealso{
\code{\link{rank_raster}}
}

