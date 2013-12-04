\docType{methods}
\name{featurenames<-}
\alias{curves}
\alias{curves,Zvariant-method}
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
\alias{performance}
\alias{performance,Zvariant-method}
\alias{plot}
\alias{plot,Zvariant,missing-method}
\alias{results}
\alias{results,Zvariant-method}
\alias{sppdata}
\alias{sppdata,Zvariant-method}
\title{Assign spp feature names to a class \code{Zvariant} instance.}
\arguments{
  \item{x}{character vector. Can be named or not.}

  \item{x}{Zvariant object.}

  \item{x}{Zvariant object.}

  \item{x}{Zvariant object.}

  \item{x}{named character vector.}

  \item{x}{Zvariant object.}

  \item{x}{Zvariant object.}

  \item{group}{boolean indicating whether to plot grouped
  curves.}

  \item{...}{Additional arguments passed on to the speficic
  plotting functions.}

  \item{x}{Zvariant object.}

  \item{x}{Zvariant object.}

  \item{group.names}{boolean indicating whether group codes
  (FALSE) or names (TRUE) are used to indicate group.
  (default: FALSE)}

  \item{x}{\code{Zresults} object.}

  \item{cols}{numeric or character vector of columns to be
  returned}

  \item{groups}{logical indicating whether group curves
  data should be returned.}

  \item{x}{\code{Zresults} object.}

  \item{pr.lost}{numeric vector containing the fractions of
  landscape lost for which the feature/group performance
  values are wanted (default: 'all').}

  \item{features}{character vector of features names to be
  extracted. Must match with feature names in curves data}

  \item{groups}{logical indicating whether group curves
  data should be used (default: FALSE).}
}
\value{
  A named character vector containing the feature names. If
  there are no groups, return NA.

  Character vector of spp feature names.

  A numeric vector containing the groups. If there are no
  groups, return NA.

  A named character vector containing the group names. If
  there are no groups, return NA.

  A named character vector containing the group names. If
  there are no groups, return NA.

  boolean value

  Zvariant object

  Zresults object. If variant doesn't have results return
  NA.

  Data frame (object's spp.data)

  Data frame containing the (selected) curves file data. If
  column names are provided, but none are found, return NA.

  Data frame containing the curves file data for selected
  fractions of landscape lost. First column is pr_lost. If
  feature names are provided and none are viable, return
  NA.
}
\description{
  This is a replacement function for variant spp feature
  names.

  Get spp feature names for a class \code{Zvariant}
  instance.

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

  In the current implementation, only performance curves
  (feature-specific or grouped) are plotted. In the future,
  plot method should handle other types of results as well.

  Since not all changes to Zvariant are reflected to its
  Zresults (e.g. feature and group names) there may quite a
  lot runtime patching going on.

  Method will also return group column with spp data if it
  exists.

  pr_lost is always included in the returned data, but
  other columns can be specified using \code{cols}
  argument.

  Get performance levels either for features or groups from
  a \code{Zresults} object.
}
\details{
  Method itself is only a thin wrapper to functions
  \code{\link{plot_curves}} and
  \code{\link{plot_grp_curves}}.
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

  Joona Lehtomaki \email{joona.lehtomaki@gmail.com}

  Joona Lehtomaki \email{joona.lehtomaki@gmail.com}
}
\seealso{
  \code{\link{Zvariant-class}} \code{\link{featurenames}}

  \code{\link{Zvariant-class}} \code{\link{groupnames}}
  \code{\link{groups}}

  \code{\link{Zvariant-class}}

  \code{\link{Zvariant-class}} \code{\link{groupnames}}
  \code{\link{groups}}

  \code{\link{Zvariant-class}} \code{\link{groupnames}}
  \code{\link{groups}}

  \code{\link{Zvariant-class}}

  \code{\link{Zvariant-class}}

  \code{\link{Zresults-class}}

  \code{\link{Zvariant-class}}

  \code{\link{Zresults-class}} \code{\link{read_curves}}
  \code{\link{read_grp_curves}}

  \code{\link{Zresults-class}} \code{\link{read_curves}}
  \code{\link{read_grp_curves}}
}

