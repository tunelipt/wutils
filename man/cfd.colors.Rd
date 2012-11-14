\name{cfd.colors}
\alias{cfd.colors}
\title{Color palette with colors varying from blue to red.}
\usage{
  cfd.colors(n)
}
\arguments{
  \item{Number}{of levels.}
}
\value{
  Vector with n colors.
}
\description{
  Color palette with colors varying from blue to red.
  Typical color scheme used in CFD visualizations.
}
\examples{
x <- cfd.colors(10)
keyWindow(1:10, x)
}
\seealso{
  \code{\link{rainbow}}
}

