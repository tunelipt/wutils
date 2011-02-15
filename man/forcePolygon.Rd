\name{forcePolygon}
\alias{forcePolygon}
\title{Force calculation}
\description{Calculates the forces and moments due to pressure acting on
  a polygonal surface.}
\usage{
forcePolygon(p, pol, coords=NULL, norm=NULL, aproj=NULL)
}
\arguments{
  \item{p}{Pressure.}
  \item{pol}{A polygon.}
  \item{coords}{Coordinates of the application point of the force. If
    \code{NULL} then it is the centroid.}
  \item{norm}{The normal to the polygon. It is the actual direction of
    application of the force. If it \code{NULL} then it is the upward
    normal with nz = 1.}
  \item{aproj}{If the polygon was projected on a plane, this is the
    factor by which the area of the polygon projection must be divided
    by in order to obtain the area of the non-projected polygon.}
}
\details{
  If projection is involved the function assumes that all original
  surfaces are flat. After getting the point of application of the
  force, the function calculates the moment also.
}

\value{A vector containing the forces and moments: \code{c(Fx, Fy, Fz,
    Mx, My, Mz)}.}
  
\author{Paulo Jabardo \email{pjabardo@ipt.br}}

\seealso{
  \code{\link{centroid.poly}}
  \code{\link{area.poly}}
}
\examples{
x <- c(0, 1, 1, 0)
y <- c(0, 0, 1, 1)
p1 <- make.poly(x, y)
p2 <- make.poly(x+2, y+2)
p <- append.poly(p1, p2)
press <- 100
forcePolygon(press, p)
}