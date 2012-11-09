\name{isStrInt}
\alias{isStrInt}
\alias{isStrNum}
\title{Is the string a number?}
\usage{
  isStrInt(s)

  isStrNum(s, dec = ".")
}
\arguments{
  \item{s}{Character array.}

  \item{dec}{Decimal point separator.}
}
\value{
  TRUE if input is a integer/number FALSE otherwise.
}
\description{
  Determines whether a string is a number (or integer)?.
}
\details{
  \code{isStrInt} determines whether a string is a integer.
  \code{isStrNum} determines whether a string is a number.
}
\examples{
s <- "XYZ"
print(isStrInt(s))
print(isStrNum(s))
s <- "-123"
print(isStrInt(s))
print(isStrNum(s))
s <- "-123.234"
print(isStrInt(s))
print(isStrNum(s))
s <- "1e4"
print(isStrInt(s))
print(isStrNum(s))
}

