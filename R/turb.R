integralScale <- function(u, spans=NULL, nn=NULL, degree=4, plt=FALSE){

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
  cat("Velocidade média:", Um, '\n')
  cat("Frequência de pico:", wmax, '\n')
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


calcLogProfile <- function(z,u, k=0.4){

  plot(u, z, log='y')
  pts <- range(identify(u,z))
  r <- pts[1]:pts[2]
  uu <- u[r]
  zz <- z[r]

  return(logProfileFit(zz,uu, k))
}

calcPowerProfile <- function(z,u, zref=1){

  plot(u, z, log='xy')
  pts <- range(identify(u,z))
  r <- pts[1]:pts[2]
  uu <- u[r]
  zz <- z[r]

  return(powerProfileFit(zz,uu, zref))
}

