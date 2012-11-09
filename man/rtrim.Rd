\name{rtrim}
\alias{ltrim}
\alias{rtrim}
\alias{trim}
\title{String trimming.}
\usage{
  rtrim(s, chars = " \n\t\r")

  ltrim(s, chars = " \n\t\r")

  trim(s, chars = " \n\t\r")
}
\arguments{
  \item{s}{Character array to be trimmed.}

  \item{chars}{Characters to be trimmed.}
}
\value{
  Trimmed version of the character array.
}
\description{
  Trims whitespace off the ends of strings.
}
\details{
  \code{rtrim} trims the right end of the string,
  \code{ltrim} trims the left end of the string and
  \code{trim} trims both ends of the string.
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
\seealso{
  \code{\link{sub}}
}

