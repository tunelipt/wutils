\name{trim}
\alias{rtrim}
\alias{ltrim}
\alias{trim}
\title{String trimming.}
\description{Trims whitespace off the ends of strings.}
\usage{
rtrim(s, chars=" \n\t\r")
ltrim(s, chars=" \n\t\r")
trim(s, chars=" \n\t\r")
}
\arguments{
\item{s}{Character array to be trimmed}
\item{chars}{Characters to be trimmed.}
}
\details{
  \code{rtrim} trims the right end of the string.

  \code{ltrim} trims the left end of a string.

  \code{trim} trims both ends of the string.
}
\value{Trimmed version of the character array.}
\seealso{
  \code{\link{sub}}
}

\examples{
s <- "   garbagexxxx"
print(rtrim(s))
print(rtrim(s, 'x'))
print(ltrim(s))
print(ltrim(s, 'x'))
print(trim(s))
print(trim(s,'x'))
print(trim(s,' x'))
}