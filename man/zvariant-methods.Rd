\docType{methods}
\name{groups}
\alias{groupnames}
\alias{groupnames,Zvariant-method}
\alias{groupnames<-}
\alias{groupnames<-,Zvariant-method}
\alias{groups}
\alias{groups,Zvariant-method}
\alias{has_results}
\alias{has_results,Zvariant-method}
\alias{plot}
\alias{plot,Zvariant,missing-method}
\title{Get group codes of a class \code{Zvariant} instance.}
\arguments{
  \item{x}{Zvariant object.}

  \item{x}{Zvariant object.}

  \item{x}{named character vector.}

  \item{x}{Zvariant object.}

  \item{x}{Zvariant object.}

  \item{group}{boolean indicating whether to plot grouped
  curves.}

  \item{...}{Additional arguments passed on to the speficic
  plotting functions.}
}
\value{
  A numeric vector containing the groups. If there are no
  groups, return NA.

  A named character vector containing the group names. If
  there are no groups, return NA.

  A named character vector containing the group names. If
  there are no groups, return NA.

  boolean value

  Zvariant object
}
\description{
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
}
\details{
  Method itself is only a thin wrapper to functions
  \code{\link{plot_curves}} and
  \code{\link{plot_grp_curves}}.
}
\author{
  Joona Lehtomaki \email{joona.lehtomaki@gmail.com}

  Joona Lehtomaki \email{joona.lehtomaki@gmail.com}

  Joona Lehtomaki \email{joona.lehtomaki@gmail.com}

  Joona Lehtomaki \email{joona.lehtomaki@gmail.com}

  Joona Lehtomaki \email{joona.lehtomaki@gmail.com}
}
\seealso{
  \code{\link{Zvariant-class}}

  \code{\link{Zvariant-class}} \code{\link{groupnames}}
  \code{\link{groups}}

  \code{\link{Zvariant-class}} \code{\link{groupnames}}
  \code{\link{groups}}

  \code{\link{Zvariant-class}}

  \code{\link{Zvariant-class}}
}

