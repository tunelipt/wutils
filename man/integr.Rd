\name{integr}
\alias{integr}
\title{Signal integrator and differentiator}
\description{Implements integrals and derivatives of sampled data using \code{\link{fft}}.}
\usage{
integr(x, p=1, dt=NULL)
}
\arguments{
  \item{x}{Array containing data to be integrate.}
  \item{p}{Order of integration. Negative integers represent derivatives.}
  \item{dt}{Time steps betweend samples. If not specified, \code{x} is
    assumed to be a time series \code{\link{ts}} object.}
}
\seealso{
  \code{\link{hpfilt}}
  \code{\link{lpfilt}}
}
\examples{
tt <- seq(0, 1, len=65)[1:64]
dt <- tt[2]
x <- ts(cos(2*pi*tt), start=0, deltat=dt)
dx <- integr(x, -1)
ix <- integr(x, 1)
plot(x, ylim=range(x, dx, ix))
lines(dx, lty=2)
lines(ix, lty=3)
legend(0, max(x,dx,ix), c("Signal", "Derivative", "Integral"), lty=1:3)
}

