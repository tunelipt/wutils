\name{powerFit}
\alias{powerFit}
\title{Power fit}
\usage{
  powerFit(x, y)
}
\arguments{
  \item{x}{Vector containing x coordinates of the points.}

  \item{y}{Vector containing y coordinates of the points.}
}
\value{
  Vector with fit coefficients.
}
\description{
  Calculates the log fit of a set of points using
  \code{\link{lm}}.
}
\details{
  This function returns the coefficients that best
  approximate the set of points according to the equation
  \deqn{y = a_1 x^a_2} The function returns the vector
  \code{c(a1, a2)}.
}
\examples{
x <- 1:10
y <- 2.5 * x^0.3 + rnorm(10, sd=0.2)

fit <- powerFit(x, y)
plot(x, y, xlab='x', ylab='y')
lines(x, fit[1]*x^fit[2])
print(fit)
}
\author{
  Paulo José Saiz Jabardo.
}
\seealso{
  \code{\link{lm}} \code{\link{predict.lm}}
}

