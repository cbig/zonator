\docType{class}
\name{Zproject}
\alias{Zproject}
\alias{Zproject-class}
\title{The Zcurves class}
\description{
  \code{Zcurves} class inherits class data.frame.

  \code{Zproject} class represents a Zonation project
  project, i.e. all the input and output files and folders.
}
\details{
  Class does no implement new methods, but it is used to
  override some behaviour such as plot. Usually
  \code{Zcurves} object belongs to a
  \code{\link[zonator:Zresults-class]{Zresults}} object.

  A project contains one or more variants of particular
  Zonation analysis setup. A single variant is represented
  as an instance of
  \code{\link[zonator:Zvariant-class]{Zvariant-class}}.
}
\section{Slots}{
  \describe{ \item{\code{groups}:}{Logical indicating if
  the curves information is from groups.}
}

\section{Slots}{
  \describe{ \item{\code{root}:}{Character string path
  pointing to the root (dir) of the project.}
  \item{\code{variants}:}{List of objects of class
  \code{\link[zonator:Zvariant-class]{Zvariant-class}}.} }
}
\author{
  Joona Lehtomaki <joona.lehtomaki@gmail.com>

  Joona Lehtomaki <joona.lehtomaki@gmail.com>
}

