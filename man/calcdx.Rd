\name{calcdx}
\alias{calcdx}
\title{Calculates a sensible interpolation increment.}
\usage{
  calcdx(dx, y = c(1, 2, 5))
}
\arguments{
  \item{dx}{Approximate rounding increment.}

  \item{y}{Nice endings for rounding increments.}
}
\value{
  Nice rounding increment to be used by
  \code{\link{rounddx}}.
}
\description{
  When interpolating to a nearest increment, some
  increments are better than others usually something that
  ends in numbers like 1, 2 or 5. This function calculates
  an appropriate increment.
}

