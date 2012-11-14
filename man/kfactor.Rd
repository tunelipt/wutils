\name{kfactor}
\alias{kfactor}
\title{Calculates the k factor.}
\usage{
  kfactor(p = 0.95, df = 5)
}
\arguments{
  \item{p}{Probability.}

  \item{df}{Degrees of freedom.}
}
\description{
  Calculates the k factor, useful when calculating
  uncertainties.
}
\examples{
k = kfactor(df=5)
print(k)
}
\seealso{
  \code{\link{qt}}
}

