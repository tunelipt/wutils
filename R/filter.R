

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


