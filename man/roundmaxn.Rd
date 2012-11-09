\name{roundmaxn}
\alias{roundmaxn}
\title{Rounds numbers to nearest level.}
\usage{
  roundmaxn(x, n = 200)
}
\arguments{
  \item{x}{Numbers to be rounded.}

  \item{n}{\code{max(x)/n} is the approximate level to
  which the numbers should be rounded.}
}
\value{
  Rounded numbers.
}
\description{
  Function that rounds a set of numbers so that the numbers
  are rounded according to fraction of the number with
  maximum absolute value.
}

