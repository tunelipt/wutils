\name{rep2}
\alias{rep2}
\title{A more specific case of \code{\link{rep}}.}
\usage{
  rep2(x, r)
}
\arguments{
  \item{x}{Vector with elements that should be repeated.}

  \item{r}{Vector of the same size as \code{x} specifiying
  how many times each element should be repeated.}
}
\value{
  Vector with repeated elements.
}
\description{
  This function is used to create e vector of repeating
  elements where the number of times each number is
  repeated can be specified independently.
}
\examples{
print(rep2(c(1,2,3), c(1,2,3)))
}

