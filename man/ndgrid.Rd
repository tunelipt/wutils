\name{ndgrid}
\alias{ndgrid}
\title{Creates n-dimensional grids.}
\usage{
  ndgrid(...)
}
\arguments{
  \item{...}{Numeric vectors. The first arguments run
  faster than later ones.}
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
g <- ndgrid(1:3, 11:14)
gg <- lapply(g, as.double)
print(cbind(gg[[1]], gg[[2]]))
h <- ndgrid(1:3, 11:14, 101:102)
hh <- lapply(h, as.double)
print(cbind(hh[[1]], hh[[2]], hh[[3]]))
}

