\docType{methods}
\name{has_results}
\alias{has_results}
\alias{has_results,Zresults-method}
\alias{has_results,Zvariant-method}
\title{Check which results a Z* object has.}
\usage{
has_results(x)

\S4method{has_results}{Zresults}(x)

\S4method{has_results}{Zvariant}(x)
}
\arguments{
  \item{x}{\code{\link[zonator]{Zvariant-class}} or
  \code{\link[zonator:Zresults-class]{Zresults}} object.}
}
\value{
list of logical values
}
\description{
If the results are availbale (i.e. variants have been run)
then the variant should have a list object containing the
results.
}
\details{
The value returned is a list of logical where key of each
element corresponds to a specific type of results.
}
\author{
Joona Lehtomaki \email{joona.lehtomaki@gmail.com}
}
\seealso{
\code{\link{Zvariant-class}}
}

