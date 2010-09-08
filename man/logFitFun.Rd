\name{logFitFun}
\alias{logFitFun}
\title{Log regression}
\description{Returns a function that computes the fitted log function of
  a set of points using \code{\link{logFit}}.}
\usage{
logFitFun(x, y)
}
\arguments{
  \item{x}{Vector containing x coordinates of the points}
  \item{y}{Vector containing y coordinates of the points}
}
\seealso{
  \code{\link{logFit}}
}
\examples{
x <- 1:10
y <- 3*log(x) + 1 + rnorm(10, sd=0.2)

fitfun <- logFitFun(x, y)
plot(x, y, xlab='x', ylab='y')
lines(x, fitfun(x))
}