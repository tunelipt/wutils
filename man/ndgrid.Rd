\name{ndgrid}
\alias{ndgrid}
\title{Creates n-dimensional grids.}
\usage{
  ndgrid(..., use.array = FALSE, use.df = TRUE)
}
\arguments{
  \item{...}{Numeric vectors. The first arguments run
  faster than later ones.}

  \item{use.array}{Return result as a list of arrays?}

  \item{use.df}{Return result as a data.frame (if
  \code{arr=FALSE})?}
}
\value{
  List with arrays.
}
\description{
  Creates an n dimensional grid given values in each
  coordinate direction.
}
\details{
  Very often, rectangular grid is necessary given the
  coordinates along each direction resulting in the
  coordinates of each point in the grid. This function
  generalizes this such that it works for n-dimensional
  cubes.
}
\examples{
g <- ndgrid(x=1:3, y=11:14)
print(g)
h <- ndgrid(x=1:3, y=11:14, z=101:102)
print(h)
w <- ndgrid(x=1:3, y=11:14, use.array=TRUE)
print(w)
}

