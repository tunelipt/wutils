% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/turb.R
\name{powerProfileFit}
\alias{powerProfileFit}
\title{Fit a velocity profile to a power law.}
\usage{
powerProfileFit(z, u, zref = 1)
}
\arguments{
\item{z}{Heights were the velocity was measured.}

\item{u}{Velocity measured.}

\item{zref}{Reference height.}
}
\value{
Vector \code{c(p, uref)}.
}
\description{
Uses the function \code{\link{powerFit}} to find the parameters
of the power law velocity profile:
\deqn{u/uref = (z/zref)^p}
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
