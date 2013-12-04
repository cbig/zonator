\docType{methods}
\name{curves
Get curves data of a given \code{Zresults} object.

pr_lost is always included in the returned data, but other columns can be
specified using \code{cols} argument.}
\alias{"featurenames<-"
Assign spp feature names to a class \code{Zvariant} instance.

This is a replacement function for variant spp feature names.}
\alias{"groupnames<-"
Assign group names to a class \code{Zvariant} instance.

This is a replacement function for variant group names. If the particular
variant doesn't use groups the gives a warning.}
\alias{curves}
\alias{curves
Get curves data of a given \code{Zresults} object.

pr_lost is always included in the returned data, but other columns can be
specified using \code{cols} argument.}
\alias{curves,Zvariant-method}
\alias{featurenames<-}
\alias{groupnames}
\alias{groupnames
Get group names for a class \code{Zvariant} instance.}
\alias{groupnames,Zvariant-method}
\alias{groupnames<-}
\alias{groupnames<-,Zvariant-method}
\alias{groups}
\alias{groups
Get group codes of a class \code{Zvariant} instance.

If the particular variant doesn't use groups or doesn't have them assigned,
return NA. Note that here 'groups' means the first column in Zonation groups
file ('output group').}
\alias{groups,Zvariant-method}
\alias{has_results}
\alias{has_results
Check if an instance of (class \code{Zvariant}) has results.

If the results are availbale (i.e. variants have been run) then the variant
should have a list object containing the results.}
\alias{has_results,Zvariant-method}
\alias{performance}
\alias{performance
Get performance levels either for features or groups from a \code{Zresults}
object.}
\alias{performance,Zvariant-method}
\alias{plot_curves}
\alias{plot_curves,Zresults,missing-method}
\alias{results}
\alias{results
Getter method for results (\code{Zresults}) in a class
\code{Zvariant} object.

Since not all changes to Zvariant are reflected to its Zresults (e.g. feature
and group names) there may quite a lot runtime patching going on.}
\alias{results,Zvariant-method}
\alias{sppdata}
\alias{sppdata
Simple getter mehtod for spp data in a class \code{Zvariant}object.

Method will also return group column with spp data if it exists.}
\alias{sppdata,Zvariant-method}
\arguments{
  \item{x}{\code{Zresults} object.}

  \item{cols}{numeric or character vector of columns to be
  returned}

  \item{groups}{logical indicating whether group curves
  data should be returned.}

  \item{x}{character vector. Can be named or not.}

  \item{x}{Zvariant object.}

  \item{x}{Zvariant object.}

  \item{x}{Zvariant object.}

  \item{value}{named character vector.}

  \item{x}{Zvariant object.}

  \item{x}{\code{Zresults} object.}

  \item{pr.lost}{numeric vector containing the fractions of
  landscape lost for which the feature/group performance
  values are wanted (default: 'all').}

  \item{features}{character vector of features names to be
  extracted. Must match with feature names in curves data}

  \item{groups}{logical indicating whether group curves
  data should be used (default: FALSE).}

  \item{x}{Zvariant object.}

  \item{x}{Zvariant object.}

  \item{group.names}{boolean indicating whether group codes
  (FALSE) or names (TRUE) are used to indicate group.
  (default: FALSE)}
}
\value{
  Data frame containing the (selected) curves file data. If
  column names are provided, but none are found, return NA.

  A named character vector containing the feature names. If
  there are no groups, return NA.

  A numeric vector containing the groups. If there are no
  groups, return NA.

  A named character vector containing the group names. If
  there are no groups, return NA.

  A named character vector containing the group names. If
  there are no groups, return NA.

  boolean value

  Data frame containing the curves file data for selected
  fractions of landscape lost. First column is pr_lost. If
  feature names are provided and none are viable, return
  NA.

  Zresults object. If variant doesn't have results return
  NA.

  Data frame (object's spp.data)
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

  \code{\link{Zvariant-class}} \code{\link{featurenames}}

  \code{\link{Zvariant-class}}

  \code{\link{Zvariant-class}} \code{\link{groupnames}}
  \code{\link{groups}}

  \code{\link{Zvariant-class}} \code{\link{groupnames}}
  \code{\link{groups}}

  \code{\link{Zvariant-class}}

  \code{\link{Zresults-class}} \code{\link{read_curves}}
  \code{\link{read_grp_curves}}

  \code{\link{Zresults-class}}

  \code{\link{Zvariant-class}}
}

