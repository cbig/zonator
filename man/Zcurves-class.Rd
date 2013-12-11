\docType{class}
\name{Zcurves}
\alias{Zcurves}
\alias{Zcurves-class}
\title{The Zcurves class}
\description{
\code{Zcurves} class inherits class data.frame.
}
\details{
Class does no implement new methods, but it is used to
override some behaviour such as plot. Usually
\code{Zcurves} object belongs to a
\code{\link[zonator:Zresults-class]{Zresults}} object.
}
\note{
If user modifies or subsets Zcurves in a funtion it is up
to the user to update the indexes in slot \code{feat.loc}
}
\section{Slots}{
  \describe{ \item{\code{groups}:}{Logical indicating if
  the curves information is from groups.}
  \item{\code{is.feature}:}{Logical indicating whether
  column is actually a feature.} }
}
\author{
Joona Lehtomaki <joona.lehtomaki@gmail.com>
}

