\name{voronoi}
\alias{voronoi}
\title{Voronoi tesselation}
\description{Calculates the Voronoi tesselation of a set of points using
  the \code{deldir} package.}
\usage{
voronoi(x, y, xb=NULL, yb=NULL)
}
\arguments{
  \item{x}{Vector containing x coordinates of the points}
  \item{y}{Vector containing y coordinates of the points}
  \item{xb}{x values of Voronoi diagram output bounding box}
  \item{yb}{y values of Voronoi diagram output bounding box}
}
\details{

  This function uses \code{\link{deldir}} and \code{\link{tile.list}}
  from package \code{deldir} to compute the Voronoi diagram of a set of
  points. The function than calls \code{\link{voronoi2polygonlst}} to
  convert to the format used by the package \code{gpclib}.
}
\value{A list of \code{gpclib} polygons that make up the Voronoi
  tesselation.}

\author{Paulo Jabardo \email{pjabardo@ipt.br}}

\seealso{
  \code{\link{deldir}}
  \code{\link{tile.list}}
  \code{\link{voronoi2polygonlst}}
}
\examples{
x <- runif(30)
y <- runif(30)
vor <- voronoi(x, y, xb=c(-1, 2), yb=c(-0.5, 1.5))
plot(x, y, xlim=c(-2, 3), ylim=c(-2,3))
n <- length(vor)
for (i in 1:n) plot(vor[[i]], add=TRUE)
}