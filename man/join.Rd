\name{join}
\alias{join}
\title{\code{\link{parse}} with better defaults}
\description{Simply a wrapper around \code{\link{parse}} with better defaults.}
\usage{
join(..., sep='', collapse=NULL)
}
\arguments{
  \item{...}{Arguments to be passed to \code{\link{paste}}}
  \item{sep}{Separator}
  \item{collapse}{Character that joins vectors}
}
\value{A character array.}

\examples{

print(join(1,2,3))
print(join(1:10))
print(join(1:10, collapse="."))

}