\docType{class}
\name{Zresults}
\alias{Zresults}
\alias{Zresults-class}
\title{The Zresults class}
\description{
\code{Zresults} class represents a full set of Zonation
results associated with a single variant (instance of class
\code{\link[zonator:Zvariant-class]{Zvariant-class}}).
}
\note{
In the current implementation all the results must be
found. Otherwise none are recorder.
}
\section{Slots}{
  \describe{ \item{\code{parent}:}{\code{Zvariant} object
  to which results belong to.}
  \item{\code{root}:}{Character string path pointing to the
  root (dir) of the results.}
  \item{\code{modified}:}{Character timestamp for when
  results were last modified.}
  \item{\code{run.info}:}{Character file path for run info
  file.} \item{\code{curves}:}{Data frame of curve
  (performance) results.} \item{\code{grp.curves}:}{Data
  frame of group curve (performance) results.}
  \item{\code{rank}:}{RasterLayer object of rank priority.}
  \item{\code{wrscr}:}{RasterLayer object of weighted
  range-size corrected richness.}
  \item{\code{prop}:}{RasterLayer object of the
  proportional loss of distribution.}
  \item{\code{has.results}:}{Logical indicating if all the
  results are there.} }
}
\author{
Joona Lehtomaki <joona.lehtomaki@gmail.com>
}

