#' Conversion between degrees and radians.
#'
#' Converts angles from degrees to radians and from radians to
#' to degrees.
#'
#' \code{d2r} Degrees -> Radians.
#' \code{r2d} Radians -> Degrees.
#'
#' @param x Numeric vector containing angle in degrees (\code{d2r}) or radians (\code{r2d}).
#' @return Angle in radians (\code{d2r}) or in degrees (\code{r2d})
#' @examples
#' ad <- 45
#' ar <- d2r(ad)
#' print(ar)
#' ad2 <- r2d(ar)
#' print(ad2)
#' @aliases d2r r2d
#' @export
d2r <- function(x) x*pi/180

#' @rdname d2r
#' @export
r2d <- function(x) x*180/pi




#' Trigonometric functions using degrees for angles.
#'
#' Implementation of trigonometric functions that uses
#' degrees instead of radians for angle arguments and
#' results.
#'
#' \code{cosd} 
#' \code{sind} 
#' \code{tand} 
#' \code{acosd} 
#' \code{asind} 
#' \code{atand}
#' \code{atan2d}
#'
#' @param x Numeric vectors.
#' @param y Numeric vectors.
#' @return Numeric vector.
#' @seealso \code{\link{cos}} \code{\link{sin}} \code{\link{tan}} \code{\link{acos}} \code{\link{asin}}
#' \code{\link{atan}} \code{\link{atan2}}
#' @aliases  cosd sind tand asind acosd atand atan2d
#' @export
cosd <- function(x) cos(d2r(x))

#' rdname cosd
#' @export
sind <- function(x) sin(d2r(x))

#' rdname cosd
#' @export
tand <- function(x) tan(d2r(x))


#' rdname cosd
#' @export
acosd <- function(x) r2d(acos(x))

#' rdname cosd
#' @export
asind <- function(x) r2d(asin(x))

#' rdname cosd
#' @export
atand <- function(x) r2d(atan(x))


#' rdname cosd
#' @export
atan2d <- function(y, x) r2d(atan2(y, x))
