\docType{methods}
\name{curves}
\alias{curves}
\alias{curves,Zresults-method}
\title{Get curves data of a given \code{Zresults} object.}
\arguments{
  \item{x}{\code{Zresults} object.}

  \item{cols}{numeric or character vector of columns to be
  returned}

  \item{groups}{logical indicating whether group curves
  data should be returned.}
}
\value{
  Data frame containing the (selected) curves file data. If
  column names are provided, but none are found, return NA.
}
\description{
  pr_lost is always included in the returned data, but
  other columns can be specified using \code{cols}
  argument.
}
\author{
  Joona Lehtomaki \email{joona.lehtomaki@gmail.com}
}
\seealso{
  \code{\link{Zresults-class}} \code{\link{read_curves}}
  \code{\link{read_grp_curves}}
}

