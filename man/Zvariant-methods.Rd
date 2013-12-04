\docType{methods}
\name{curves}
\alias{curves}
\alias{featurenames}
\alias{featurenames,Zvariant-method}
\alias{featurenames<-}
\alias{groupnames}
\alias{groupnames,Zvariant-method}
\alias{groupnames<-}
\alias{groupnames<-,Zvariant-method}
\alias{groups}
\alias{groups,Zvariant-method}
\alias{has_results}
\alias{has_results,Zvariant-method}
\alias{plot_curves}
\alias{plot_curves,Zresults,missing-method}
\alias{results}
\alias{results,Zvariant-method}
\alias{sppdata}
\alias{sppdata,Zvariant-method}
\title{Get curves data of a given \code{Zresults} object.}
\arguments{
  \item{x}{\code{Zresults} object.}

  \item{cols}{numeric or character vector of columns to be
  returned}

  \item{groups}{logical indicating whether group curves
  data should be returned.}

  \item{x}{Zvariant object.}

  \item{x}{character vector. Can be named or not.}

  \item{x}{Zvariant object.}

  \item{x}{Zvariant object.}

  \item{x}{Zvariant object.}

  \item{value}{named character vector.}

  \item{x}{Zvariant object.}

  \item{x}{Zvariant object.}

  \item{x}{Zvariant object.}

  \item{group.names}{boolean indicating whether group codes
  (FALSE) or names (TRUE) are used to indicate group.
  (default: FALSE)}
}
\value{
  Data frame containing the (selected) curves file data. If
  column names are provided, but none are found, return NA.

  Character vector of spp feature names.

  A named character vector containing the feature names. If
  there are no groups, return NA.

  A numeric vector containing the groups. If there are no
  groups, return NA.

  A named character vector containing the group names. If
  there are no groups, return NA.

  A named character vector containing the group names. If
  there are no groups, return NA.

  boolean value

  Zresults object. If variant doesn't have results return
  NA.

  Data frame (object's spp.data)
}
\description{
  pr_lost is always included in the returned data, but
  other columns can be specified using \code{cols}
  argument.

  Get spp feature names for a class \code{Zvariant}
  instance.

  This is a replacement function for variant spp feature
  names.

  If the particular variant doesn't use groups or doesn't
  have them assigned, return NA. Note that here 'groups'
  means the first column in Zonation groups file ('output
  group').

  Get group names for a class \code{Zvariant} instance.

  This is a replacement function for variant group names.
  If the particular variant doesn't use groups the gives a
  warning.

  If the results are availbale (i.e. variants have been
  run) then the variant should have a list object
  containing the results.

  Since not all changes to Zvariant are reflected to its
  Zresults (e.g. feature and group names) there may quite a
  lot runtime patching going on.

  Method will also return group column with spp data if it
  exists.
}
\note{
  spp features have by default names that are derived from
  the feature raster file path.
}
\author{
  Joona Lehtomaki \email{joona.lehtomaki@gmail.com}

  Joona Lehtomaki \email{joona.lehtomaki@gmail.com}

  Joona Lehtomaki \email{joona.lehtomaki@gmail.com}

  Joona Lehtomaki \email{joona.lehtomaki@gmail.com}

  Joona Lehtomaki \email{joona.lehtomaki@gmail.com}

  Joona Lehtomaki \email{joona.lehtomaki@gmail.com}

  Joona Lehtomaki \email{joona.lehtomaki@gmail.com}

  Joona Lehtomaki \email{joona.lehtomaki@gmail.com}

  Joona Lehtomaki \email{joona.lehtomaki@gmail.com}
}
\seealso{
  \code{\link{Zresults-class}} \code{\link{read_curves}}
  \code{\link{read_grp_curves}}

  \code{\link{Zvariant-class}} \code{\link{groupnames}}
  \code{\link{groups}}

  \code{\link{Zvariant-class}} \code{\link{featurenames}}

  \code{\link{Zvariant-class}}

  \code{\link{Zvariant-class}} \code{\link{groupnames}}
  \code{\link{groups}}

  \code{\link{Zvariant-class}} \code{\link{groupnames}}
  \code{\link{groups}}

  \code{\link{Zvariant-class}}

  \code{\link{Zresults-class}}

  \code{\link{Zvariant-class}}
}

