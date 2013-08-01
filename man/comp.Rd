\name{comp}
\alias{comp}
\title{Compare matrices in various ways.}
\usage{
  comp(x, y, fun = "correlation", ...)
}
\arguments{
  \item{x}{numeric matrix}

  \item{y}{numeric matrix}

  \item{FUN}{function used for the numeric comparison}

  \item{...}{further args passed on to selected comparison
  function}
}
\value{
  A DataFrame with each row containg columns title, count,
  and catid
}
\description{
  Function can be used to compare two Zonation output
  rasters with one of the following functions (part of
  zonator package): \item{correlation}{}
  \item{substraction}{} \item{ferquency}{(NOT IMPLEMENTED)}
  \item{coverage}{}
}
\author{
  Joona Lehtomaki <joona.lehtomaki@gmail.com>
}
\seealso{
  correlation substraction frequency coverage
}
\keyword{post-processnig,}
\keyword{ppa}

