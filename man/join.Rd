\name{join}
\alias{join}
\title{\code{\link{parse}} with better defaults.}
\usage{
  join(..., sep = "", collapse = NULL)
}
\arguments{
  \item{...}{Arguments to be passed to
  \code{\link{paste}}.}

  \item{sep}{Separator.}

  \item{collapse}{Character that joins vectors.}
}
\value{
  A character vector.
}
\description{
  This function is now superseeded by \code{\link{paste0}}!
}
\details{
  Simply a wrapper around \code{\link{parse}} where the
  default separator is an empty character. Uses the same
  parameters as \code{\link{parse}}.
}
\examples{
print(join(1,2,3))
print(join(1:10))
print(join(1:10, collapse="."))
}
\seealso{
  \code{\link{paste0}}
}

