\name{makeColorFun}
\alias{makeColorFun}
\title{Generate a color interpolation function.}
\usage{
  makeColorFun(xmin, xmax, n = 100, palette = cfd.colors,
    underflow = NULL, overflow = NULL)
}
\arguments{
  \item{xmin}{Minimum value of the color scale.}

  \item{xmax}{Maximim value of the color scale.}

  \item{n}{Number of intermediate colors.}

  \item{palette}{Color palette used.}

  \item{underflow}{Color to use for smaller values, first
  color if NULL.}

  \item{underflow}{Color to use for largerr values, first
  color if NULL.}
}
\value{
  Function that returns a color given a numeric value.
}
\description{
  Generates a function that calculates a color scale.
}
\details{
  In filled contour plots, a color corresponds to a value.
  This function creates a new function that given a value,
  returns the corresponding color. This function can use
  several different color palettes and can handle values
  outside the values limits.
}
\examples{
fun <- makeColorFun(0, 1, underflow='black', overflow='white')
x <- c(-1, seq(0, 1, len=5), 2)
xcol <- fun(x)
keyWindow(x, xcol)
fun <- makeColorFun(0, 1)
xcol <- fun(x)
keyWindow(x, xcol)
}

