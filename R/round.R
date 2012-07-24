
#' Rounds number to nearest increment.
#'
#' Instead of rounding a number to the nearest nth
#' decimal place, this function rounds to the neares
#' multiple of \code{dx}.
#'
#' @param x Number to be rounded.
#' @param dx Increment to be rounded.
#' @return Rounded number.
#' @export
rounddx <- function(x, dx=1){

  return(round(x/dx)*dx)
}


#' Calculates a sensible interpolation increment.
#'
#' When interpolating to a nearest increment, some increments
#' are better than others usually something that ends in
#' numbers like 1, 2 or 5. This function calculates an appropriate
#' increment.
#'
#' @param dx Approximate rounding increment.
#' @param y Nice endings for rounding increments.
#' @return Nice rounding increment to be used by \code{\link{rounddx}}.
#' @export
calcdx <- function(dx, y=c(1,2,5)){

  ldx <- log10(dx)
  dldx <- ldx - floor(ldx)

  ly <- log10(y)
  errmin <- ly[which.min(abs(ly - dldx))]
  ldx <- floor(ldx) + errmin

  return(10**ldx)
}


#' Rounds numbers to nearest level.
#'
#' Function that rounds a set of numbers so that the numbers are rounded according to
#' fraction of the number with maximum absolute value.
#'
#' @param x Numbers to be rounded.
#' @param n \code{max(x)/n} is the approximate level to which the numbers should be rounded.
#' @return Rounded numbers.
#' @export
roundmaxn <- function(x, n=200){

  dx <- calcdx(max(abs(x)) / n)
  
  return(rounddx(x, dx))
}

