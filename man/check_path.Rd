\name{check_path}
\alias{check_path}
\title{A function to deal with potentially relative paths.}
\usage{
  check_path(x, parent.path = NULL)
}
\arguments{
  \item{x}{Character string path.}

  \item{parent.path}{Character string root path.}
}
\value{
  A cleaned character string
}
\description{
  Checks if a path can be resolved (i.e. whether it
  exists). A additional parameter \code{parent.path} can be
  provided, in which case \code{x} is appended to it and
  the concatenated path is checked for existence. If the
  path cannot be resolved, raise an error.
}
\examples{
\dontrun{

  }
}
\author{
  Joona Lehtomaki \email{joona.lehtomaki@gmail.com}
}

