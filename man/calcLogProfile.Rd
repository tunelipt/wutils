\name{calcLogProfile}
\alias{calcLogProfile}
\title{Interactively fit of the law of the wall on measured velocity profile.}
\usage{
  calcLogProfile(z, u, k = 0.4, return.fun = FALSE,
    v = NULL, h = NULL, ...)
}
\arguments{
  \item{z}{Heights of velocity measurements.}

  \item{u}{Velocity measurements.}

  \item{k}{Von Karman constant.}

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
  velocity profile using \code{\link{logProfileFit}}.
}
\details{
  A measured velocity profile is usually not entirely a log
  layer. Near the ground (or within the roughness elements)
  or near the top of the boundary layer the velocity
  profile deviates from the law of the wall and therefore
  the applicable portion of the velocity profile should be
  specified. This function is a wrapper around
  \code{\link{logProfileFit}} where the velocity profile is
  plotted on a log-linear axis so that the law of the wall
  section is easily found. The user then specifies the
  section where the law of the wall is applicable and uses
  this section to compute the profile characteristics.
}
\examples{
z <- seq(5, 300, by=5)
k <- 0.4
z0 <- 2
us <- 0.5
u <- 1/k * log(z/z0) + rnorm(10, sd=0.2)

fit <- calcLogProfile(z, u)
plot(u, z, xlab='Height (mm)', ylab='Velocity (m/s)', ty='b')
lines(fit['us']/k * log(z/fit['z0']), z)
print(fit)
}
\seealso{
  \code{\link{logProfileFit}} \code{\link{logFitFun}}
  \code{\link{powerFitFun}}
}

