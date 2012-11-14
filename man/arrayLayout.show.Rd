\name{arrayLayout.show}
\alias{arrayLayout.show}
\title{Shows the layout of a single page.}
\usage{
  arrayLayout.show(lay, page)
}
\arguments{
  \item{lay}{Layout returned by
  \code{\link{getArrayLayout}}}

  \item{page}{Page to be plotted.}
}
\description{
  Shows the layout of a page calculated using
  \code{\link{getArrayLayout}}. Basically an example of how
  the functions \code{\link{getArrayLayout}},
  \code{\link{arrayLayout}} and ' \code{\link{legend}}. It
  plots random data using the layout of a single page.
}
\examples{
lay <- getArrayLayout(6, 2, 2)
arrayLayout.show(lay, 1)
arrayLayout.show(lay, 2)
}

