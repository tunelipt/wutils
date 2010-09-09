\name{isStrInt}
\alias{isStrInt}
\alias{isStrNum}
\title{Is string a number?}
\description{Determines whether a string is a number.}
\usage{
isStrInt(s)
isStrNum(s)
}
\arguments{
\item{s}{Character array}
}
\details{
  \code{isStrInt} determines whether a string is a integer. 

  \code{isStrNum} determines whether a string is a number.

}
\value{\code{TRUE} or \code{FALSE}.}

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