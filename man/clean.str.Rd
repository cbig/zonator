\name{clean.str}
\alias{clean.str}
\title{clean.str
A function to deal with potentially relative paths.}
\usage{
  clean.str(x)
}
\arguments{
  \item{x}{character string}
}
\value{
  A cleaned character string
}
\description{
  Checks if a path can be resolved (i.e. whether it
  exists). A additional parameter `parent.path` can be
  provided, in which case `x` is appended to it and the
  concatenated path is checked for existence.
}
\author{
  Joona Lehtomaki \email{joona.lehtomaki@gmail.com}
}

