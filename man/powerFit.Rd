\name{powerFit}
\alias{powerFit}
\title{Power law fitting}
\description{Calculates the power fit of a set of points using \code{\link{linearFit}}.}
\usage{
powerFit(x, y)
}
\arguments{
  \item{x}{Vector containing x coordinates of the points}
  \item{y}{Vector containing y coordinates of the points}
}
\seealso{
  \code{\link{logFit}}
  \code{\link{linearFit}}
}
\examples{
x <- 1:10
y <- 2.5 * x^0.3 + rnorm(10, sd=0.2)

fit <- powerFit(x, y)
plot(x, y, xlab='x', ylab='y')
lines(x, fit[1]*x^fit[2])
print(fit)
}