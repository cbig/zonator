% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllClasses.R
\docType{class}
\name{ZCurvesDataFrame}
\alias{ZCurvesDataFrame}
\alias{ZCurvesDataFrame-class}
\title{The ZCurvesDataFrame class}
\description{
\code{ZCurvesDataFrame} class inherits class data.frame.
}
\details{
Class does no implement new methods, but it is used to override some
behaviour such as plot. Usually \code{ZCurvesDataFrame} object belongs to a
\code{\link[zonator:Zresults-class]{Zresults}} object.
}
\note{
If user modifies or subsets ZCurvesDataFrame in a funtion it is up to
the user to update the indexes in slot \code{is.feature}
}
\section{Slots}{

 \describe{
   \item{\code{is.feature}:}{Logical indicating whether column is actually
   a feature.}
 }
}

\author{
Joona Lehtomaki <joona.lehtomaki@gmail.com>
}
