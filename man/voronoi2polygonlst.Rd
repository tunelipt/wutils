\name{voronoi2polygonlst}
\alias{voronoi2polygonlst}
\title{\code{gpclib} polygon list creation from \code{deldir} Voronoi tesselation}
\description{Converts a Voronoi tesselation obtained using
  \code{\link{tile.list}} into a list of \code{gpclib} polygons for
  further processing.}
\usage{
voronoi2polygonlst(vor)
}
\arguments{
  \item{vor}{A Voronoi tesselation obtained from \code{\link{tile.list}}}
}
\value{A list of \code{gpclib} polygons.}
\author{Paulo Jabardo \email{pjabardo@ipt.br}}

\seealso{
  \code{\link{deldir}}
  \code{\link{tile.list}}
  \code{\link{voronoi}}
}
\examples{
x <- runif(30)
y <- runif(30)
tri <- deldir(x, y)
vor <- tile.list(tri)
vor2 <- voronoi2polygonlst(vor)
plot(x, y)
n <- length(vor2)
for (i in 1:n) plot(vor2[[i]], add=TRUE)

}