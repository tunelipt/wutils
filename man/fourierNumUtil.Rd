\name{fourierNumUtil}
\alias{fourierNumUtil}
\title{Number of independent values in DFT of real signals.}
\usage{
  fourierNumUtil(N)
}
\arguments{
  \item{N}{Number of samples in a signal.}
}
\value{
  Number of indepent (complex) values in the DFT of the
  signal.
}
\description{
  This function calculates the number of independent values
  in Discrete Fourier Transform calculations of real
  signals.
}
\details{
  When calculating the Discrete Fourier Transform (DFT) of
  real signals, there is a symmetry on the resulting
  values: \deqn{X_k = X^*_{N-k}} where X is the discrete
  Fourier transform of the signal. The number of
  independent values in the signal depends on whether the
  length of the signal is even or odd. This function
  returns the correct number for any case.
}
\examples{
print(fourierNumUtil(10))
print(fourierNumUtil(11))
}

