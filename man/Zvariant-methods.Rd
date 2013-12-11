\docType{methods}
\name{featurenames<-}
\alias{featurenames<-}
\alias{groupnames}
\alias{groupnames,Zvariant-method}
\alias{groupnames<-}
\alias{groupnames<-,Zvariant,character-method}
\alias{groupnames<-,Zvariant-method}
\alias{groups}
\alias{groups,Zvariant-method}
\alias{has_results}
\alias{has_results,Zvariant-method}
\alias{plot,Zcurves,missing-method}
\alias{plot_curves,Zcurves,missing,logical-method}
\alias{results}
\alias{results,Zvariant-method}
\alias{sppdata}
\alias{sppdata,Zvariant-method}
\title{Assign spp feature names to a class \code{Zvariant} instance.}
\usage{
featurenames(x) <- value

groups(x)

groupnames(x)

groupnames(x) <- value

has_results(x)

results(x)

sppdata(x, group.names = FALSE)

\S4method{plot}{Zcurves,missing}(x, groups = FALSE, min = FALSE,
  mean = FALSE, w.mean = FALSE, features = NULL, monochrome = FALSE,
  invert.x = FALSE, main = "", ...)

\S4method{groups}{Zvariant}(x)

\S4method{groupnames}{Zvariant}(x)

\S4method{has_results}{Zvariant}(x)

\S4method{results}{Zvariant}(x)

\S4method{sppdata}{Zvariant}(x, group.names = FALSE)
}
\arguments{
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

  \item{...}{Arguments to be passed to methods, such as
  \link{graphical parameters} (see \code{\link{par}}).
  Many methods will accept the following arguments:
  \describe{ \item{\code{type}}{what type of plot should be
  drawn.  Possible types are \itemize{ \item \code{"p"} for
  \bold{p}oints, \item \code{"l"} for \bold{l}ines, \item
  \code{"b"} for \bold{b}oth, \item \code{"c"} for the
  lines part alone of \code{"b"}, \item \code{"o"} for both
  \sQuote{\bold{o}verplotted}, \item \code{"h"} for
  \sQuote{\bold{h}istogram} like (or \sQuote{high-density})
  vertical lines, \item \code{"s"} for stair \bold{s}teps,
  \item \code{"S"} for other \bold{s}teps, see
  \sQuote{Details} below, \item \code{"n"} for no plotting.
  } All other \code{type}s give a warning or an error;
  using, e.g., \code{type = "punkte"} being equivalent to
  \code{type = "p"} for S compatibility.  Note that some
  methods, e.g. \code{\link{plot.factor}}, do not accept
  this.  } \item{\code{main}}{an overall title for the
  plot: see \code{\link{title}}.} \item{\code{sub}}{a sub
  title for the plot: see \code{\link{title}}.}
  \item{\code{xlab}}{a title for the x axis: see
  \code{\link{title}}.} \item{\code{ylab}}{a title for the
  y axis: see \code{\link{title}}.} \item{\code{asp}}{the
  \eqn{y/x} aspect ratio, see \code{\link{plot.window}}.} }
  }
}
\value{
A named character vector containing the feature names. If
there are no groups, return NA.

A numeric vector containing the groups. If there are no
groups, return NA.

A named character vector containing the group names. If
there are no groups, return NA.

A named character vector containing the group names. If
there are no groups, return NA.

boolean value

Zresults object. If variant doesn't have results return NA.

Data frame (object's spp.data)
}
\description{
This is a replacement function for variant spp feature
names.

If the particular variant doesn't use groups or doesn't
have them assigned, return NA. Note that here 'groups'
means the first column in Zonation groups file ('output
group').

Get group names for a class \code{Zvariant} instance.

This is a replacement function for variant group names. If
the particular variant doesn't use groups the gives a
warning.

If the results are availbale (i.e. variants have been run)
then the variant should have a list object containing the
results.

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
}
\seealso{
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

