\name{check.path}
\alias{check.path}
\title{check.path
Clean leading and trailing whitespaces from a given string. Additionally,
all occurrences of multiple whitespaces are replaced with a single
whitespace.}
\usage{
  check.path(x, parent.path = NULL)
}
\arguments{
  \item{x}{character string file path}

  \item{x}{character string dir path}
}
\value{
  An absolute path to a file of NULL if the path does not
  exist
}
\description{
  check.path Clean leading and trailing whitespaces from a
  given string. Additionally, all occurrences of multiple
  whitespaces are replaced with a single whitespace.
}
\author{
  Joona Lehtomaki \email{joona.lehtomaki@gmail.com}
}

