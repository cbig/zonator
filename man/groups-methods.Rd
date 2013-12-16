\docType{methods}
\name{groups}
\alias{groups}
\alias{groups,Zvariant-method}
\title{Get group codes of a class \code{Zvariant} instance.}
\usage{
groups(x)

\S4method{groups}{Zvariant}(x)
}
\arguments{
  \item{x}{Zvariant object.}
}
\value{
A numeric vector containing the groups. If there are no
groups, return NA.
}
\description{
If the particular variant doesn't use groups or doesn't
have them assigned, return NA. Note that here 'groups'
means the first column in Zonation groups file ('output
group').
}
\author{
Joona Lehtomaki \email{joona.lehtomaki@gmail.com}
}
\seealso{
\code{\link{Zvariant-class}}
}

