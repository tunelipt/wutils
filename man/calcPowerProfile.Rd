\name{calcPowerProfile}
\alias{calcPowerProfile}
\title{Power law fitting specific for sections of velocity profiles}
\description{Calculates the power fit to a velocity profile using \code{\link{powerProfileFit}}.}
\usage{
calcPowerProfile(z,u,zref=1)
}
\arguments{
  \item{z}{Heights of velocity measurements.}
  \item{u}{Velocity measurements.}
  \item{zref}{Reference height.}
}
\details{
  A measured velocity profile is usually not entirely a power law. Near
  the ground (or within the roughness elements) or near the top of the
  boundary layer the  velocity profile  deviates from the 'main' power
  law and therefore the applicable portion of the velocity profile
  should be specified. This function is a wrapper around
  \code{\link{powerProfileFit}} where the velocity profile is plotted on
  a log-log axis so that the power law section it is easily found. The
  user then specifies the section where the power law is applicable and
  uses this section to compute the prfile characteristics.
}
\seealso{
  \code{\link{powerProfileFit}}
  \code{\link{logProfileFit}}
  \code{\link{calcLogProfile}}
}
\examples{
z <- seq(10, 400, by=10)
zref <- 300
u <- 10*(z/zref)^0.2 + rnorm(10, sd=0.2)

fit <- calcPowerProfile(z,u,zref=300)
plot(u, z, xlab='Velocity (m/s)', ylab='Height (mm)')
lines(fit['u']*(z/zref)^fit['p'], z)
print(fit)
}