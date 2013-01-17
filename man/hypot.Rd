\name{hypot}
\alias{hypot}
\title{Calculates the hypotenuse of the arguments.}
\usage{
  hypot(...)
}
\arguments{
  \item{...}{Numeric vectors.}
}
\value{
  Numeric vector.
}
\description{
  Calculates the n dimensional hypotenuse of the arguments.
  It calculates the Euclidean norm of the arguments.
}
\details{
  The hypotenuse of a 2D triangle is given by \code{h =
  sqrt(x^2 + y^2)}. In the n dimensional case, keep adding
  the terms...
}
\examples{
print(hypot(3, 4))
print(hypot(3,4,12))
}

