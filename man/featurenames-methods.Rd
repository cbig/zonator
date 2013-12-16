\docType{methods}
\name{featurenames}
\alias{featurenames}
\alias{featurenames,Zcurves-method}
\alias{featurenames,Zresults-method}
\alias{featurenames,Zvariant-method}
\alias{featurenames<-}
\alias{groupnames<-,Zvariant,character-method}
\title{Feature names of Zonation variant.}
\usage{
featurenames(x)

featurenames(x) <- value

\S4method{featurenames}{Zcurves}(x)

\S4method{featurenames}{Zresults}(x)

\S4method{featurenames}{Zvariant}(x)
}
\arguments{
  \item{x}{Z* object.}

  \item{x}{character vector. Can be named or not.}
}
\value{
Character vector of spp feature names.

A named character vector containing the feature names. If
there are no groups, return NA.
}
\description{
Getter function to retrieve names for analysis features
used a given Zonation variant.

This is a replacement function for variant spp feature
names.
}
\details{
Argument \code{x} can be an instance of one the following
Z* classes: \itemize{ \item{\code{Zvariant}}
\item{\code{Zresults}} }
}
\note{
spp features have by default names that are derived from
the feature raster file path.
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

Joona Lehtomaki \email{joona.lehtomaki@gmail.com}
}
\seealso{
\code{\link{Zvariant-class}} \code{\link{Zresults-class}}
\code{\link{groupnames}} \code{\link{groups}}

\code{\link{Zvariant-class}} \code{\link{Zresults-class}}
\code{\link{featurenames}}
}

