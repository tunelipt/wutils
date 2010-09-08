\name{powerFitFun}
\alias{powerFitFun}
\title{Power law fitting}
\description{Creates a function that calculates  the power fit of a set of points using \code{\link{powerFit}}.}
\usage{
powerFitFun(x, y)
}
\arguments{
  \item{x}{Vector containing x coordinates of the points}
  \item{y}{Vector containing y coordinates of the points}
}
\seealso{
  \code{\link{powerFit}}
  \code{\link{logFitFun}}
}
\examples{
x <- 1:10
y <- 2.5 * x^0.3 + rnorm(10, sd=0.2)

fitfun <- powerFitFun(x, y)
plot(x, y, xlab='x', ylab='y')
lines(x, fitfun(x))
}