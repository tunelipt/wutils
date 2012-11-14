\name{hpfilt}
\alias{hpfilt}
\title{High pass filter.}
\usage{
  hpfilt(x, freq, dt = NULL)
}
\arguments{
  \item{x}{Array containing data to be filtered.}

  \item{freq}{Frequency above which data will be filtered.}

  \item{dt}{Time steps betweend samples. If not specified,
  \code{x} is assumed to be a time series \code{\link{ts}}
  object.}}
}
\value{
  The filtered signal
}
\description{
  Uses \code{link{fft}} to compute a high pass filter to
  the data.
}
\examples{
tt <- seq(0, 4, len=513)[1:512]
 dt <- tt[2]
 x = ts(cos(4*pi*tt) + 1.5*sin(2*pi*tt) + 3 + cos(6*2*pi*tt),start=0,
 deltat=dt)
 xhp <- hpfilt(x, 1.5)
 plot(x, ylim=range(x, xlp))
 lines(xhp, lty=2)
legend(0, max(x, xhp), c("Signal", "Filtered Signal"), lty=c(1,2))
}

