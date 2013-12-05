\docType{methods}
\name{performance}
\alias{performance}
\alias{performance,Zresults-method}
\alias{plot_curves}
\alias{plot_curves,Zresults,missing-method}
\title{Get performance levels either for features or groups from a \code{Zresults}
object.}
\arguments{
  \item{x}{\code{Zresults} object.}

  \item{pr.lost}{numeric vector containing the fractions of
  landscape lost for which the feature/group performance
  values are wanted (default: 'all').}

  \item{features}{character vector of features names to be
  extracted. Must match with feature names in curves data}

  \item{groups}{logical indicating whether group curves
  data should be used (default: FALSE).}

  \item{x}{Zresults object.}

  \item{...}{Additional arguments passed on to the speficic
  plotting functions.}
}
\value{
  Data frame containing the curves file data for selected
  fractions of landscape lost. First column is pr_lost. If
  feature names are provided and none are viable, return
  NA.

  ggplot2 object
}
\description{
  Get performance levels either for features or groups from
  a \code{Zresults} object.

  Plot Zonation performance curves.
}
\author{
  Joona Lehtomaki \email{joona.lehtomaki@gmail.com}

  Joona Lehtomaki \email{joona.lehtomaki@gmail.com}
}
\seealso{
  \code{\link{Zresults-class}} \code{\link{read_curves}}
  \code{\link{read_grp_curves}}

  \code{\link{Zvariant-class}}
}

