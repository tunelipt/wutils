\name{polygonArea}
\alias{polygonArea}
\alias{polygonCentroid}
\title{Polygon area and centroid calculation}
\description{Calculates the area and centroid of simple polygons from their coordinates}
\usage{
polygonArea(x, y, lam=1)
polygonCentroid(x, y, lam=1)
}
\arguments{
  \item{x}{Vector containing x coordinates of the points}
  \item{y}{Vector containing y coordinates of the points}
  \item{lam}{Scale used}
}
\details{
  \code{polygonArea} calculates the area of a simple polygon from the
  coordinates of its vertices. By simple polygon, it is meant a polygon
  simply connected with no holes.
}

\value{The area or centroid of the polygon.}
  
\author{Paulo Jabardo \email{pjabardo@ipt.br}}

\seealso{
  \code{\link{area.poly}}
}
\examples{
x <- c(0, 1, 1, 0)
y <- c(0, 0, 1, 1)

A <- polygonArea(x, y, lam=1)
centr <- polygonCentroid(x, y, lam=1)
plot(c(x, x[1]), c(y, y[1]), ty='l')
points(centr[1], centr[2], pch=4)

}