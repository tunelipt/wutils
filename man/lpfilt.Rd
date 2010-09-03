\name{lpfilt}
\alias{lpfilt}
\title{Low pass filter}
\description{Implements a low pass filter using \code{\link{fft}}.}
\usage{
lpfilt(x, freq, dt=NULL)
}
\arguments{
  \item{x}{Array containing data to be filtered.}
  \item{freq}{Frequency below which data will be filtered.}
  \item{dt}{Time steps betweend samples. If not specified, \code{x} is
    assumed to be a time series \code{\link{ts}} object.}
}
\seealso{
  \code{\link{hpfilt}}
  \code{\link{ìntegr}}
}
\examples{
  tt <- seq(0, 4, len=513)[1:512]
  dt <- tt[2]
  x = ts(cos(4*pi*tt) + 1.5*sin(2*pi*tt) + 3 + cos(6*2*pi*tt),start=0,
  deltat=dt)
  xlp <- lpfilt(x, 1.5)
  plot(x, ylim=range(x, xlp))
  lines(xlp, lty=2)
  legend(0, max(x, xlp), c("Signal", "Filtered Signal"), lty=c(1,2))

}