\docType{methods}
\name{featurenames}
\alias{featurenames}
\alias{featurenames,Zcurves-method}
\alias{featurenames,Zresults-method}
\alias{featurenames,Zvariant-method}
\title{Feature names a for Zonation variant.}
\usage{
featurenames(x)

\S4method{featurenames}{Zcurves}(x)

\S4method{featurenames}{Zresults}(x)

\S4method{featurenames}{Zvariant}(x)
}
\arguments{
  \item{x}{Z* object.}
}
\value{
Character vector of spp feature names.
}
\description{
Getter function to retrieve names for analysis features
used a given Zonation variant.
}
\details{
Argument \code{x} can be an instanve of one the following
Z* classes: \itemize{ \item{\code{Zvariant}}
\item{\code{Zresults}} }
}
\examples{
setup.dir <- system.file("extdata/tutorial/basic", package="zonator")
tutorial.project <- create_zproject(setup.dir)
variant.caz <- get_variant(tutorial.project, "01_core_area_zonation")

# Feature names for a Zvariant object
featurenames(variant.caz)

# Feature names for a Zresults object
results.caz <- results(variant.caz)
featurenames(results.caz)
}
\author{
Joona Lehtomaki \email{joona.lehtomaki@gmail.com}
}
\seealso{
\code{\link{Zvariant-class}} \code{\link{Zresults-class}}
\code{\link{groupnames}} \code{\link{groups}}
}

