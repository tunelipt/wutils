\name{logFitFun}
\alias{logFitFun}
\title{Create function that calculates the log fit.}
\usage{
  logFitFun(x, y)
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
  of a set of points using \code{\link{logFit}}.
}
\examples{
x <- 1:10
y <- 3*log(x) + 1 + rnorm(10, sd=0.2)

fitfun <- logFitFun(x, y)
plot(x, y, xlab='x', ylab='y')
lines(x, fitfun(x))
}
\seealso{
  \code{\link{logFit}}
}

