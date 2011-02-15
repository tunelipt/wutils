\name{centroid.poly}
\alias{centroid.poly}
\title{Polygon centroid calculation}
\description{Calculates the centroid of polygons from their coordinates}
\usage{
centroid.poly(p)
}
\arguments{
  \item{p}{A \code{gpclib} polygon}
}
\details{
  Uses \code{\link{polygonCentroid}} and \code{\link{polygonArea}} to
  calculate the centroids and areas of each sub-polygon and then
  combines it all to calculate the centroid of the whole set. Polygons
  with holes are not being handled yet.
}

\value{The centroid of the polygon.}
  
\author{Paulo Jabardo \email{pjabardo@ipt.br}}

\seealso{
  \code{\link{polygonCentroid}}
}
\examples{
x <- c(0, 1, 1, 0)
y <- c(0, 0, 1, 1)
p1 <- make.poly(x, y)
p2 <- make.poly(x+2, y+2)
p <- append.poly(p1, p2)
cc <- centroid.poly(p)
plot(p)
points(cc[1], cc[2], pch=4)

}