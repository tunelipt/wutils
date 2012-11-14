\name{calcPowerProfile}
\alias{calcPowerProfile}
\title{Interactively fit a power law on measured velocity profile.}
\usage{
  calcPowerProfile(z, u, zref = 1, return.fun = FALSE,
    v = NULL, h = NULL, ...)
}
\arguments{
  \item{z}{Heights of velocity measurements.}

  \item{u}{Velocity measurements.}

  \item{zref}{Reference height.}

  \item{return.fun}{Return fit parameters or a function
  that calculates the velocity?}

  \item{v}{Any vertical lines that should be plotted.}

  \item{h}{Any horizontal lines that should be plotted.}

  \item{...}{Any parameters that shoud be passed along to
  \code{\link{chooseFitSection}}.}
}
\value{
  Fit parameters or function depending on argument
  \code{return.fun}.
}
\description{
  Calculates the law of the wall that best fits to a
  velocity profile using \code{\link{powerProfileFit}}.
}
\details{
  A measured velocity profile is usually not entirely a
  power law. Near the ground (or within the roughness
  elements) or near the top of the boundary layer the
  velocity profile deviates from the power law and
  therefore the applicable portion of the velocity profile
  should be specified. This function is a wrapper around
  \code{\link{powerProfileFit}} where the velocity profile
  is plotted on a log-log graph so that the power law
  section is easily identifiable. The user then specifies
  the section where the law of the wall is applicable and
  uses this section to compute the profile characteristics.
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
\seealso{
  \code{\link{logProfileFit}} \code{\link{logFitFun}}
  \code{\link{powerFitFun}}
}

