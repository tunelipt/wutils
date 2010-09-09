\name{numString}
\alias{numString}
\title{Integer to string converter}
\description{Converts an integer into a number with given number of
  digits. If necessary, prepends the number with zeros to fill the digits.}
\usage{
numString(x, n=3)
}
\arguments{
  \item{x}{Number}
  \item{n}{Number of digits to be used}
}
\value{A string containing the number with \code{n} characters}

\examples{

print(numString(1:10, 2))
print(numString(1:10, 4))

}