\docType{methods}
\name{groupnames}
\alias{groupnames}
\alias{groupnames,ZGroupCurvesDataFrame-method}
\alias{groupnames,Zresults-method}
\alias{groupnames,Zvariant-method}
\alias{groupnames<-}
\alias{groupnames<-,Zvariant,character-method}
\title{Get group names for a class \code{Zvariant} instance.}
\usage{
groupnames(x)

\S4method{groupnames}{ZGroupCurvesDataFrame}(x)

\S4method{groupnames}{Zresults}(x)

\S4method{groupnames}{Zvariant}(x)
}
\arguments{
  \item{x}{Zvariant object.}
}
\value{
A character vector containing the group names. If there are
no groups, return NA.
}
\description{
Get group names for a class \code{Zvariant} instance.
}
\author{
Joona Lehtomaki \email{joona.lehtomaki@gmail.com}
}
\seealso{
\code{\link{Zvariant-class}} \code{\link{groupnames}}
\code{\link{groups}}
}

