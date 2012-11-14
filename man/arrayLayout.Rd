\name{arrayLayout}
\alias{arrayLayout}
\title{Creates the layout of a page.}
\usage{
  arrayLayout(page, lay, respect = FALSE)
}
\arguments{
  \item{page}{Page number to get layout.}

  \item{lay}{Layout structure calculated with
  \code{\link{getArrayLayout}}.}

  \item{Analogous}{to respect parameter in
  \code{\link{layout}} function.}
}
\description{
  Uses the layout calculated by
  \code{\link{getArrayLayout}} to create the layout of a
  page.
}
\examples{
lay <- getArrayLayout(6, 2, 2)
arrayLayout(1, lay)
plot(1:10)
plot(1:10)
plot(1:10)
plot(1:10)
legendWindow("center", legend="Points", pch=1)
arrayLayout(2, lay)
plot(1:10)
plot(1:10)
legendWindow("center", legend="Points", pch=1)
}
\seealso{
  getArrayLayout layout
}

