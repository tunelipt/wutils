\name{powerFitFun}
\alias{powerFitFun}
\title{Create function that calculates the power fit.}
\usage{
  powerFitFun(x, y)
}
\arguments{
  \item{x}{Vector containing x coordinates of the points.}

  \item{y}{Vector containing y coordinates of the points.}
}
\value{
  Function that uses the fit parameters to estimate y.
}
\description{
  Returns a function that computes the fitted log function
  of a set of points using \code{\link{powerFit}}.
}
\examples{
x <- 1:10
y <- 2.5 * x^0.3 + rnorm(10, sd=0.2)

fitfun <- powerFitFun(x, y)
plot(x, y, xlab='x', ylab='y')
lines(x, fitfun(x))
}
\seealso{
  \code{\link{powerFit}}
}

