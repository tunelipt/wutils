

#' Low pass filter.
#'
#' Uses \code{link{fft}} to compute a low pass filter to the data.
#'
#' @param x Array containing data to be filtered.
#' @param freq Frequency below which data will be filtered.
#' @param dt Time steps betweend samples. If not specified, \code{x} is assumed to be a time series \code{\link{ts}} object.}
#' @return The filtered signal
#' @examples
#'  tt <- seq(0, 4, len=513)[1:512]
#'  dt <- tt[2]
#'  x = ts(cos(4*pi*tt) + 1.5*sin(2*pi*tt) + 3 + cos(6*2*pi*tt),start=0,
#'  deltat=dt)
#'  xlp <- lpfilt(x, 1.5)
#'  plot(x, ylim=range(x, xlp))
#'  lines(xlp, lty=2)
#' legend(0, max(x, xlp), c("Signal", "Filtered Signal"), lty=c(1,2))
#' @export
lpfilt <- function(x, freq, dt=NULL){

  N <- length(x)
  dtnull <- is.null(dt)
  X <- fft(x)/N

  if (dtnull) dt <- deltat(x)
  period <- N*dt
  df <- 1/period
  f <- (0:(N-1))*df
  nm1 <- N-1
  nf <- freq %/% df

  X[(nf+2):(N-nf)] <- 0.0
  xfilt <- Re(fft(X, inv=TRUE))
  if (dtnull) xfilt <- ts(xfilt, start=0, deltat=dt)
  return(xfilt)
}


#' High pass filter.
#'
#' Uses \code{link{fft}} to compute a high pass filter to the data.
#'
#' @param x Array containing data to be filtered.
#' @param freq Frequency above which data will be filtered.
#' @param dt Time steps betweend samples. If not specified, \code{x} is assumed to be a time series \code{\link{ts}} object.}
#' @return The filtered signal
#' @examples
#'  tt <- seq(0, 4, len=513)[1:512]
#'  dt <- tt[2]
#'  x = ts(cos(4*pi*tt) + 1.5*sin(2*pi*tt) + 3 + cos(6*2*pi*tt),start=0,
#'  deltat=dt)
#'  xhp <- hpfilt(x, 1.5)
#'  plot(x, ylim=range(x, xlp))
#'  lines(xhp, lty=2)
#' legend(0, max(x, xhp), c("Signal", "Filtered Signal"), lty=c(1,2))
#' @export
hpfilt <- function(x, freq, dt=NULL){

  
  N <- length(x)
  dtnull <- is.null(dt)
  X <- fft(x)/N

  if (dtnull) dt <- deltat(x)
  period <- N*dt
  df <- 1/period
  f <- (0:(N-1))*df
  nm1 <- N-1
  nf <- freq %/% df

  X[1:(nf+1)] <- 0.0
  X[(N-nf+1):N] <- 0.0
  
  xfilt <- Re(fft(X, inv=TRUE))
  if (dtnull) xfilt <- ts(xfilt, start=0, deltat=dt)
  return(xfilt)
}

#' Band pass filter.
#'
#' Uses \code{link{fft}} to compute a band pass filter to the data.
#'
#' @param x Array containing data to be filtered.
#' @param freq1 Lower frequency above which data will be filtered.
#' @param freq2 Higher frequency below which data will be filtered.
#' @param dt Time steps betweend samples. If not specified, \code{x} is assumed to be a time series \code{\link{ts}} object.}
#' @return The filtered signal
#' @examples
#'  tt <- seq(0, 4, len=513)[1:512]
#'  dt <- tt[2]
#'  x = ts(cos(4*pi*tt) + 1.5*sin(2*pi*tt) + 3 + cos(6*2*pi*tt),start=0,
#'  deltat=dt)
#'  xhp <- hpfilt(x, 1.5)
#'  plot(x, ylim=range(x, xlp))
#'  lines(xhp, lty=2)
#' legend(0, max(x, xhp), c("Signal", "Filtered Signal"), lty=c(1,2))
#' @export
bandfilt <- function(x, freq1, freq2, dt=NULL){
  N <- length(x)
  dtnull <- is.null(dt)
  X <- fft(x)/N

  if (dtnull) dt <- deltat(x)
  period <- N*dt
  df <- 1/period
  f <- (0:(N-1))*df
  nm1 <- N-1
  nf1 <- freq1 %/% df
  nf2 <- freq2 %/% df
  faixa1 <- (nf1+2):(nf2)
  faixa2 <- N-faixa1
  
  X[faixa1] <- 0.0
  X[faixa2] <- 0.0

  xfilt <- Re(fft(X, inv=TRUE))
  if (dtnull) xfilt <- ts(xfilt, start=0, deltat=dt)
  return(xfilt)
}


  
#' Signal integrator and differentiator.
#'
#' Implements integrals and derivatives of sampled data using \code{\link{fft}}.
#'
#' @param x Array containing data to be integrate.
#' @param p Order of integration. Negative integers represent derivatives.
#' @param dt Time steps betweend samples. If not specified, \code{x} is assumed to be a time series \code{\link{ts}} object.
#' @return A vector The integral of the signal.
#' @examples
#' tt <- seq(0, 1, len=65)[1:64]
#' dt <- tt[2]
#' x <- ts(cos(2*pi*tt), start=0, deltat=dt)
#' dx <- integr(x, -1)
#' ix <- integr(x, 1)
#' plot(x, ylim=range(x, dx, ix))
#' lines(dx, lty=2)
#' lines(ix, lty=3)
#' legend(0, max(x,dx,ix), c("Signal", "Derivative", "Integral"), lty=1:3)
#' @export
integr <- function(x, p=1, dt=NULL){

  N <- length(x)
  dtnull <- is.null(dt)
  X <- fft(x)/N

  if (dtnull) dt <- deltat(x)
  period <- N*dt
  df <- 1/period
  f <- (0:(N-1))*df

  X[0] <- 0.0  # Necessário. Se não não funciona (x é periódico!!!)
  nd2 <- N %/% 2 + 1
  rest <- N-nd2
  base <- c(0, ((1:(nd2-1))*(2*pi*1i/period))^(-p))
  
  base2 <- Conj(rev(base[2:(1+rest)]))
  coefs <- c(base, base2)
  xint <- Re(fft(X*coefs, inv=TRUE))
  if (dtnull) xint <- ts(xint, start=0, deltat=dt)
  return(xint)
}


