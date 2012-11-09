\name{integralScale}
\alias{integralScale}
\title{Integral length scale calculation of turbulent flows.}
\usage{
  integralScale(u, spans = NULL, nn = NULL, degree = 4,
    plt = FALSE, verbose = TRUE)
}
\arguments{
  \item{u}{Time series containing velocity.}

  \item{spans}{Filtering parameters to be passed to
  \code{\link{spec.pgram}}.}

  \item{nn}{Number of times of the peak frequency to use to
  clip the spectrum.}

  \item{degree}{Degree of polynomial that will be fitted to
  the spectrum.}

  \item{plt}{Whether the spectrum should be plot.}

  \item{verbose}{Print calculation info?}
}
\value{
  Integral length scale.
}
\description{
  Calculates the integral length scale.
}
\details{
  Uses equation XYZ from Semiu and Scanlan book to compute
  the integral length scale: \deqn{L =
  \frac{1}{2\pi}\frac{\bar{U}}{f_\text{peak}}}{L = 1/(2.pi)
  Um / f_peak} The spectrum is computed using
  \code{\link{spec.pgram}} and parameter \code{spans} is
  passed to this function to smooth the spectrum. If
  parameter \code{nn} is \code{NULL}, all points of the
  spectrum is fitted. If it is an integer, the point os
  maximum spectral density is found
  (\eqn{f_\text{peak}}{f_peak}) and the spectrum is fitted
  using only points whose frequency is smaller than
  \eqn{nn\cdot f_\text{peak}}{nn.f_peak}.

  The log of spectral density and log of frequency are than
  fitted to a \code{degree} degrees polynomial and the peak
  frequency is calculated and used to calculate the
  integral length scale.
}
\author{
  Paulo José Saiz Jabardo
}
\seealso{
  \code{\link{spec.pgram} \code{\link{lm}}
}

