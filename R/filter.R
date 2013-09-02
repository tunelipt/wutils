

#' Low pass filter.
#'
#' Uses \code{link{mvfft}} to compute a low pass filter to the data.
#'
#' @param x Vector or matrix containing data to be filtered. If matrix, each column corresponds to a time series.
#' @param freq Frequency below which data will be filtered.
#' @param dt Time steps betweend samples. If not specified, \code{x} is assumed to be a time series \code{\link{ts}} object.
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
  dx <- dim(x)
  dtnull <- is.null(dt)
  if (dtnull) dt <- deltat(x)

  x <- as.matrix(x)
  
  N <- nrow(x)
  X <- mvfft(x)/N

  period <- N*dt
  df <- 1/period
  f <- (0:(N-1))*df
  nm1 <- N-1
  nf <- freq %/% df

  X[(nf+2):(N-nf),] <- 0.0
  xfilt <- Re(mvfft(X, inv=TRUE))
  dim(xfilt) <- dx
  if (dtnull) xfilt <- ts(xfilt, start=0, deltat=dt)
  return(xfilt)
}


#' High pass filter.
#'
#' Uses \code{link{mvfft}} to compute a high pass filter to the data.
#'
#' @param x Vector or matrix containing data to be filtered. If matrix, each column corresponds to a time series.
#' @param freq Frequency above which data will be filtered.
#' @param dt Time steps betweend samples. If not specified, \code{x} is assumed to be a time series \code{\link{ts}} object.
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

  
  dx <- dim(x)
  dtnull <- is.null(dt)
  if (dtnull) dt <- deltat(x)

  x <- as.matrix(x)
  
  N <- nrow(x)
  X <- mvfft(x)/N

  period <- N*dt
  df <- 1/period
  f <- (0:(N-1))*df
  nm1 <- N-1
  nf <- freq %/% df

  X[1:(nf+1),] <- 0.0
  X[(N-nf+1):N,] <- 0.0
  
  xfilt <- Re(mvfft(X, inv=TRUE))
  dim(xfilt) <- dx
  if (dtnull) xfilt <- ts(xfilt, start=0, deltat=dt)
  return(xfilt)
}

#' Band  filter.
#'
#' Uses \code{link{mvfft}} to compute a band filter to the data.
#'
#' @param x Vector or matrix containing data to be filtered. If matrix, each column corresponds to a time series.
#' @param freq1 Lower frequency above which data will be filtered.
#' @param freq2 Higher frequency below which data will be filtered.
#' @param dt Time steps betweend samples. If not specified, \code{x} is assumed to be a time series \code{\link{ts}} object.
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

  dx <- dim(x)
  dtnull <- is.null(dt)
  if (dtnull) dt <- deltat(x)

  x <- as.matrix(x)
  
  N <- nrow(x)
  X <- mvfft(x)/N

  period <- N*dt
  df <- 1/period
  f <- (0:(N-1))*df
  nm1 <- N-1
  nf1 <- freq1 %/% df
  nf2 <- freq2 %/% df
  faixa1 <- (nf1+2):(nf2)
  faixa2 <- N-faixa1
  
  X[faixa1,] <- 0.0
  X[faixa2,] <- 0.0

  xfilt <- Re(mvfft(X, inv=TRUE))
  dim(xfilt) <- dx
  if (dtnull) xfilt <- ts(xfilt, start=0, deltat=dt)
  return(xfilt)
}


#' Band pass filter.
#'
#' Uses \code{link{mvfft}} to compute a band pass filter to the data.
#'
#' @param x Vector or matrix containing data to be filtered. If matrix, each column corresponds to a time series.
#' @param freq1 Lower frequency below which data will be filtered.
#' @param freq2 Higher frequency above which data will be filtered.
#' @param dt Time steps betweend samples. If not specified, \code{x} is assumed to be a time series \code{\link{ts}} object.
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
bandpass <- function(x, freq1, freq2, dt=NULL)
  hpfilt(lpfilt(x, freq2, dt), freq1, dt)




  
#' Signal integrator and differentiator.
#'
#' Implements integrals and derivatives of sampled data using \code{\link{mvfft}}.
#'
#' Note that the function assumes that the signal has zero mean. The nonzer mean is not integrated.
#'
#' @param x Vector or matrix containing data to be integrated. If matrix, each column corresponds to a time series.
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
  dx <- dim(x)
  dtnull <- is.null(dt)
  if (dtnull) dt <- deltat(x)

  x <- as.matrix(x)
  
  N <- nrow(x)
  X <- mvfft(x)/N

  period <- N*dt
  df <- 1/period
  f <- (0:(N-1))*df
  X[1,] <- 0.0  # Necessário. Se não não funciona (x é periódico!!!)
  nd2 <- N %/% 2 + 1
  rest <- N-nd2
  base <- c(0, ((1:(nd2-1))*(2*pi*1i/period))^(-p))
  
  base2 <- Conj(rev(base[2:(1+rest)]))
  coefs <- c(base, base2)
  X <- apply(X, 2, function(y) y*coefs)
  xint <- Re(mvfft(X, inv=TRUE))
  dim(xint) <- dx
  if (dtnull) xint <- ts(xint, start=0, deltat=dt)
  return(xint)
}

#' Number of independent values in DFT of real signals.
#'
#' This function calculates the number of independent values
#' in Discrete Fourier Transform calculations of real signals.
#'
#' When calculating the Discrete Fourier Transform (DFT)
#' of real signals, there is a symmetry on the resulting
#' values:
#' \deqn{X_k = X^*_{N-k}}
#' where X is the discrete Fourier transform of the signal.
#' The number of independent values in the signal depends on whether the length
#' of the signal is even or odd. This function returns the correct number for any case.
#'
#' @param N Number of samples in a signal.
#' @return Number of indepent (complex) values in the DFT of the signal.
#' @examples
#' print(fourierNumUtil(10))
#' print(fourierNumUtil(11))
#' @export
fourierNumUtil <- function(N) 1 + ceiling( (N-1)/2 )


#' Resamples a data set
#'
#' This function uses trigonometric interpolation to resample a time series.
#'
#' Sometimes it is interesting to resample a time series so that a greater
#' resolution is available. This function uses trigonometric interpolation
#' to resample the time series. The parameter \code{m} is the number of
#' additional points that will be added to the signal. The function also
#' works on arrays of data where each column represents a time series.
#'
#' @param x Vector or matrix of data to be resampled.
#' @param m Number of additional points to be added.
#' @return Resampled data. If a \code{\link{ts}} object was used, a new object with the resampled frequency is returned.
#' @examples
#' x <- seq(0, 1, len=11)
#' y <- ts(sin(2*pi*x)[1:10], start=0, deltat=x[2]-x[1])
#' y2 <- resample(y, 30)
#' x1 <- seq(0, 1, len=201)
#' y1 <- sin(2*pi*x1)
#' plot(x1, y1, xlab="Time", ylab=expression(sin(2*pi*t)), ty='l')
#' points(y, ty='p')
#' points(y2, pch=3, col=3)
#' legend("topright", c(expression(sin(2*pi*t)), "Original signal", "Resampled signal"), pch=c(-1, 1,3), col=c(1, 1,3), lty=c(1, -1,-1))
#' @export
resample <- function(x, m){
  dx <- dim(x)
  xts <- FALSE
  if (is.ts(x)){
    xts <- TRUE
    st <- start(x)
    freq <- frequency(x)
  }

  
  x <- as.matrix(x)
  N <- nrow(x)
  
  
  X <- 1/N * mvfft(x)

  Nutil <- fourierNumUtil(N)
  X1 <- X[1:Nutil,, drop=FALSE]
  X2 <- X[(Nutil+1):N,, drop=FALSE]

  pad <- complex(m*ncol(x))
  dim(pad) <- c(m, ncol(x))
  Xnew <- rbind(X1, pad, X2)

  xnew <- Re(mvfft(Xnew, inv=TRUE))

  if (is.null(dx)) dim(xnew) <- NULL
  if (xts) xnew <- ts(xnew, start=st, freq=freq*(N + m) / N)
  
  return(xnew)
}
