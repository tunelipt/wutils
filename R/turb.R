#' Integral length scale calculation of turbulent flows
#'
#' Calculates the integral length scale.
#'
#'  Uses equation XYZ from Semiu and Scanlan book to compute the integral
#'  length scale:
#'
#'  \deqn{L = \frac{1}{2\pi}\frac{\bar{U}}{f_\text{peak}}}{L = 1/(2.pi) Um / f_peak}
#'
#' The spectrum is computed using \code{\link{spec.pgram}} and parameter
#' \code{spans} is passed to this function to smooth the spectrum. If
#' parameter \code{nn} is \code{NULL}, all points of the spectrum is
#' fitted. If it is an integer, the point os maximum spectral density is
#' found (\eqn{f_\text{peak}}{f_peak}) and the spectrum is fitted using only points whose
#' frequency is smaller than \eqn{nn\cdot f_\text{peak}}{nn.f_peak}.
#'
#' The log of spectral density and log of frequency are than fitted to a
#' \code{degree} degrees polynomial and the peak frequency is calculated
#'  and used to calculate the integral length scale.
#'
#' @param u Time series containing velocity.
#' @param spans Filtering parameters to be passed to \code{\link{spec.pgram}}.
#' @param nn Number of times of the peak frequency to use to clip the spectrum.
#' @param degree Degree of polynomial that will be fitted to the spectrum.
#' @param plt Whether the spectrum should be plot.
#' @param verbose Print calculation info?
#' @return Integral length scale.
#' @seealso \code{\link{spec.pgram}  \code{\link{lm}} 
#' @authot Paulo José Saiz Jabardo <pjabardo@ipt.br>
#' @export
integralScale <- function(u, spans=NULL, nn=NULL, degree=4, plt=FALSE, verbose=TRUE){

  Um <- mean(u)
  
  S <- spec.pgram(u, spans=spans, plot=FALSE)
  w <- S[[1]]
  S <- w*S[[2]]
  n1 <- length(w)
  npeak <- which.max(S)
  if (is.null(nn))
    n <- n1
  else
    n <- min(n1, nn*npeak)
  S <- S[1:n]
  w <- w[1:n]
  lnw <- log(w)
  lns <- log(S)
  
  model <- lm(lns ~ poly(lnw, degree), weights=1/(1:n))
  ss <- exp(predict.lm(model, data.frame(lnw)))
  npeak <- which.max(ss)
  ww <- seq(w[npeak-1], w[npeak+1], len=50)
  smax <- exp(predict.lm(model, data.frame(lnw=log(ww))))
  wmax <- ww[which.max(smax)]
  if (plt){
    plot(w, S, ty='l', log='xy')
    lines(w, ss, col='red', lwd=2)
    abline(v=wmax)
  }
  L <- 1/(2*pi) * Um / wmax
  if (verbose){
    cat("Velocidade média:", Um, '\n')
    cat("Frequência de pico:", wmax, '\n')
  }
  return(L)
}


linearFit <- function(x,y){
  fit <- as.vector(lm(y ~ x)[[1]])
  return(fit)
}

logFit <- function(x,y){

  lnx <- log(x)

  fit <- as.vector(lm(y ~ lnx)[[1]])
  return(fit)
}
logFitFun <- function(x,y){
  f <- logFit(x,y)
  return(function(x)f[1] + f[2]*log(x))
}
logProfileFit <- function(z,u, k=0.4){

  f <- logFit(z,u)
  us <- f[2]*k
  z0 <- exp(-f[1]/f[2])

  return(c(z0=z0, us=us))
}

powerFit <- function(x,y){

  lnx <- log(x)
  lny <- log(y)
  fit <- linearFit(lnx, lny)
  a <- exp(fit[1])
  b <- fit[2]

  return(c(a=a, b=b))
}

powerFitFun <- function(x,y){
  fit <- powerFit(x,y)
  return(function(x) fit[1]*x^fit[2])
}

powerProfileFit <- function(z,u, zref=1){
  z <- z/zref
  fit <- as.double(powerFit(z,u))
  b <- fit[2]
  uref <- fit[1]

  return(c(p=b, u=uref))
}


chooseFitSection <- function(x, y, v=NULL, h=NULL, ...){
  plot(x, y, ty='p', ...)

  if (!is.null(v)) abline(v=v)
  if (!is.null(h)) abline(h=h)

  return(range(identify(x,y)))
}

calcLogProfile <- function(z,u, k=0.4, return.fun=FALSE, v=NULL, h=NULL,...){

  
  plot(u, z, log='y')
  pts <- chooseFitSection(u,z, log='y', xlab='Velocity', ylab='Height', v=v,
                          h=h, ...)
  #pts <- range(identify(u,z))
  r <- pts[1]:pts[2]
  uu <- u[r]
  zz <- z[r]

  f <- logProfileFit(zz,uu, k)
  fun <- function(z) f[2]/k * log(z/f[1])
  
  lines(fun(z), z)
  lines(fun(zz), zz, col='red', lwd=3)
  if (return.fun)
    return(fun) 
  else
    return(f)
}

calcPowerProfile <- function(z,u, zref=1, return.fun=FALSE, ...){

  plot(u, z, log='xy')
  pts <- chooseFitSection(u,z, log='xy', xlab='Velocity', ylab='Height', ...)
  #pts <- range(identify(u,z))
  r <- pts[1]:pts[2]
  uu <- u[r]
  zz <- z[r]

  f <- powerProfileFit(zz,uu, zref)
  fun <- function(z) f[2]*(z/zref)**f[1]
    
  lines(fun(z), z)
  lines(fun(zz), zz, col='red', lwd=3)
  if (return.fun)
    return(fun) 
  else
    return(f)
  return(f)
}

