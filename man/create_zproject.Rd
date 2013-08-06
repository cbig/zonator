\name{create_zproject}
\alias{create_zproject}
\title{Create a Zonation project.}
\usage{
  create_zproject(root, variants, dat.from = NULL,
    spp.from = NULL)
}
\arguments{
  \item{root}{Character string path pointing either to an
  existing directory (with potentially bat-files in it) or
  to a new directory to be created.}

  \item{variants}{Character vector of names for new
  variants. Ignored if using an existing project.}

  \item{dat.from}{Character path to a dat file template. If
  no template is specified, uses the template distributed
  with zonator. Ignored if using an existing project.}

  \item{spp.from}{Character path to a spp file template. If
  no template is specified, uses the template distributed
  with zonator. Ignored if using an existing project.}
}
\value{
  A Zproject object.
}
\description{
  Project can be created based on a set of existing
  bat-files by pointing the root of he project to an
  existing directory containing the bat-files. In this case
  each bat-file is converted into a Zvariant-object.
}
\details{
  Alternatively an empty project can be created by
  supplying a root path that does not exist as an argument.
}
\examples{
\dontrun{

  }
}
\author{
  Joona Lehtomaki \email{joona.lehtomaki@gmail.com}
}
\seealso{
  \code{\link[zonator:Zproject-class]{Zproject-class}} and
  \code{\link[zonator:Zvariant-class]{Zvariant-class}}
}

