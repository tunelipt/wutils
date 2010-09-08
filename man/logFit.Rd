\name{logFit}
\alias{logFit}
\title{Log regression}
\description{Calculates the log fit of a set of points using \code{\link{lm}}.}
\usage{
logFit(x, y)
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

fit <- logFit(x, y)
plot(x, y, xlab='x', ylab='y')
lines(x, fit[1] + fit[2]*log(x))
print(fit)
}