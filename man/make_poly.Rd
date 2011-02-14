\name{make.poly}
\alias{make.poly}
\title{Polygon creation}
\description{Helper function to create a \code{gpclib} polygon from the
  node coordinates.}
\usage{
make.poly(x, y)
}
\arguments{
  \item{x}{Vector containing x coordinates of the points}
  \item{y}{Vector containing y coordinates of the points}
}

\value{A \code{gpclib} polygon.}
\author{Paulo Jabardo \email{pjabardo@ipt.br}}

\seealso{
  \code{\link{voronoi2polygonlst}}
}
\examples{
p <- make.poly(c(0, 1, 1, 0), c(0, 0, 1, 1))
plot(p)
}