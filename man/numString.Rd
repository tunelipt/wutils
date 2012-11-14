\name{numString}
\alias{numString}
\title{Integer to string converter}
\usage{
  numString(x, n = 3)
}
\arguments{
  \item{Integer}{to be converted to string.}

  \item{n}{number of digits to be used.}
}
\value{
  String containing the number padded on its left with 0s
  \code{n} characters long.
}
\description{
  Converts an integer into a string with a given digits. If
  necessary, the function prepends the number with zeros to
  fill the digits.
}
\details{
  When creating numbered file names, the file system lists
  the files alphabetically. This is usually a nuisance
  since the listing is out of order: file_1, file_10,
  file_11, ..., file_2. This function makes it easy to
  create file names such as file_01, file_02, ..., file_09,
  file_10, file_11, ....
}
\examples{
print(numString(1:10, 2))
print(numString(1:10, 4))
}

