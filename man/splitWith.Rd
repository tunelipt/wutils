\name{splitWith}
\alias{splitWith}
\title{Splitting a list of polygons using another polygon}
\description{Computes the intersection of each polygon in a list of
  polygons with a polygon.}
\usage{
voronoi(vor, hull)
}
\arguments{
  \item{vor}{List of polygons}
  \item{hull}{Polygon with which each polygon in \code{vor} will be intersected}
}
\details{
  This function was written so that a Voronoi tesselation could be fit
  into a polygon. Basically, on its fringes a Voronoi volume
  theoratically goes to infinity and if it is desired to fit the
  tesselation inside a larger polygon this function can be used to chop
  off the parts of the volumes (each polygon in \code{vor} outside this
  larger polygon (\code{hull}).
}

\value{A list containing the intersection of each polygon in \code{vor}
  with \code{hull}.}

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
hull <- make.poly(c(0, 1, 1, 1.5, 1.5, 0), c(0, 0, 0.7, 0.7, 1, 1))
lst <- splitWith(vor, hull)
plot(hull)
points(x, y)
n <- length(lst)
for (i in 1:n) plot(lst[[i]], add=TRUE)
}