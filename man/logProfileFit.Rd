\name{logProfileFit}
\alias{logProfileFit}
\title{Fit a log velocity profile.}
\usage{
  logProfileFit(z, u, k = 0.4)
}
\arguments{
  \item{z}{Vector containing the heights of the boundary
  layer.}

  \item{u}{Vector containing the velocities of the boundary
  layer.}

  \item{k}{Von Karman constant.}
}
\value{
  Vector \code{c(z0,us)}
}
\description{
  Calculates the log fit of a velocity profile using
  \code{\link{logFit}}.
}
\details{
  This function calculates the the parameters of a velocity
  profile according to the law of the wall:
  \deqn{\frac{u}{u_*} = \frac{1}{\kappa} \ln
  \frac{z}{z_0}}{u/u* = 1/k * ln(z/z0)} This function
  returnd u* (us) and z0 given k (usually 0.4 or 0.41).
}
\examples{
z <- seq(5, 300, by=5)
k <- 0.4
z0 <- 2
us <- 0.5
u <- 1/k * log(z/z0) + rnorm(10, sd=0.2)

fit <- logProfileFit(z, u)
plot(u, z, xlab='Height (mm)', ylab='Velocity (m/s)', ty='b')
lines(fit['us']/k * log(z/fit['z0']), z)
print(fit)
}
\seealso{
  \code{\link{logFit}}
}

