% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graphutils.R
\name{keyWindow}
\alias{keyWindow}
\title{Plots a color keymap.}
\usage{
keyWindow(levels, colors, side = 4, border = NULL, mar = rep(0.2, 4),
  spc = 1, barw = 0.5, ...)
}
\arguments{
\item{levels}{Values that should be plotted.}

\item{colors}{Corresponding colors.}

\item{side}{On which side should the legend be plotted.}

\item{border}{Color of the border.}

\item{mar}{Size of the margins.}

\item{spc}{A parameter that contracts (spc < 1) or expands (z>1) the size of the keymap.}

\item{barw}{Width of the keymap.}

\item{...}{Parameters passed to \code{\link{text}} function.}
}
\description{
Plots a standalone keymap on its own window.
}
\examples{
keyWindow(1:10, side=4)
keyWindow(1:10, side=1)
keyWindow(1:10, side=4, spc=0.6)
keyWindow(1:10, side=4, barw=0.2)
}
\seealso{
\code{\link{rect}} \code{\link{text}} \code{\link{contourf}}
}
