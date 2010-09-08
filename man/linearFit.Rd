\name{linearFit}
\alias{linearFit}
\title{Linear regression}
\description{Calculates the linear fit of a set of points using \code{\link{lm}}.}
\usage{
linearFit(x, y)
}
\arguments{
  \item{x}{Vector containing x coordinates of the points}
  \item{y}{Vector containing y coordinates of the points}
}
\seealso{
  \code{\link{lm}}
  \code{\link{predict.lm}}
}
\examples{
x <- 1:10
y <- 2*x + rnorm(10, sd=0.4)

fit <- linearFit(x, y)
plot(x, y, xlab='x', ylab='y')
lines(x, fit[1] + fit[2]*x)
print(fit)
}