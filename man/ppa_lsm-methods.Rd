\docType{methods}
\name{ppa_lsm}
\alias{ppa_lsm}
\alias{ppa_lsm,Zresults-method}
\title{Get ppa.lsm results data of a Z* object.}
\usage{
ppa_lsm(x)

\S4method{ppa_lsm}{Zresults}(x)
}
\arguments{
  \item{x}{Z* object.}
}
\value{
Data frame containing PPA LSM data items 1 and 3 combined
(See \code{\link{Zresults-class}} for more details).
}
\description{
Simple getter-method for ppa.lsm information (if used)
contained in Zonation results
}
\author{
Joona Lehtomaki \email{joona.lehtomaki@gmail.com}
}
\seealso{
\code{\link{Zresults-class}} \code{\link{read_curves}}
\code{\link{read_grp_curves}}
}
\keyword{results}

