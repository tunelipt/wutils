\name{powerProfileFit}
\alias{powerProfileFit}
\title{Power law fitting specific for velocity profiles}
\description{Calculates the poewr fit to a velocity profile using \code{\link{powerFit}}.}
\usage{
powerProfileFit(z,u,zref=1)
}
\arguments{
  \item{z}{Heights of velocity measurements.}
  \item{u}{Velocity measurements.}
  \item{zref}{Reference height.}
}
\seealso{
  \code{\link{powerFit}}
  \code{\link{logProfileFit}}
  \code{\link{powerFitFun}}
}
\examples{
z <- seq(10, 400, by=10)
zref <- 300
u <- 10*(z/zref)^0.2 + rnorm(10, sd=0.2)

fit <- powerProfileFit(z,u,zref=300)
plot(u, z, xlab='Velocity (m/s)', ylab='Height (mm)')
lines(fit['u']*(z/zref)^fit['p'], z)
print(fit)
}