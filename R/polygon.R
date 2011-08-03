require(gpclib)
require(deldir)

voronoi <- function(x,y, xb=NULL, yb=NULL){
  if (length(x)==1){
    if (is.null(xb)) xb <- c(x-10, x+10)
    if (is.null(yb)) yb <- c(y-10, y+10)

    xx <- c(xb[1], xb[2], xb[2], xb[1])
    yy <- c(yb[1], yb[1], yb[2], yb[2])

    return(list(as(cbind(xx, yy), 'gpc.poly')))
  }

  if (is.null(xb)){
    rx <- range(x)
    dx <-rx[2] - rx[1]
    xb <- c(rx[1] - 1.2*dx, rx[2] + 1.2*dx)
  }

  if (is.null(yb)){
    ry <- range(y)
    dy <-ry[2] - ry[1]
    yb <- c(ry[1] - 1.2*dy, ry[2] + 1.2*dy)
  }


    
  tri <- deldir(x,y, rw=c(xb[1], xb[2], yb[1], yb[2]))

  
  return(voronoi2polygonlst(tile.list(tri)))

}

make.poly <- function(x, y)   as(cbind(x, y), 'gpc.poly')

  

voronoi2polygonlst <- function(vor){

  return(lapply(vor, function(v) as(cbind(v$x, v$y), 'gpc.poly')))
}


splitWith <- function(vor, hull){

  return(lapply(vor, function(v) intersect(v, hull)))
}



polygonArea <- function(x, y, lam=1){
  n <- length(x)
  x <- c(x, x[1])
  y <- c(y, y[1])
  area <- (x[1:n]*y[2:(n+1)] - x[2:(n+1)]*y[1:n])
  area <- 0.5*(sum(area)) * lam^2
  return(area)
  
}

polygonCentroid <- function(x, y, lam=1){
  A <- polygonArea(x, y, lam)
  n <- length(x)
  x <- c(x, x[1])
  y <- c(y, y[1])
  
  cx <- (x[1:n]+x[2:(n+1)]) * (x[1:n]*y[2:(n+1)] - x[2:(n+1)]*y[1:n])
  cx <- 1/(6*A) * sum(cx)
  cy <- (y[1:n]+y[2:(n+1)]) * (x[1:n]*y[2:(n+1)] - x[2:(n+1)]*y[1:n])
  cy <- 1/(6*A) * sum(cy)

  return(c(cx, cy))
}

centroid.poly <- function(p){
  pts <- get.pts(p)
  np <- length(pts)

  A <- sapply(pts, function(p) polygonArea(p$x, p$y))
  C <- sapply(pts, function(p) polygonCentroid(p$x, p$y))
  #hole <- sapply(pts, function(p) p$hole)
  #fhole <- double(np)
  #fhole[hole] <- -1
  #fhloe[!hole] <- 1
  
  xc <- sum(C[1,] * A) / sum(A)
  yc <- sum(C[2,] * A) / sum(A)

  return(c(xc, yc))
}
  
  
forcePolygon <- function(p, pol, coords=NULL, norm=NULL, aproj=NULL){
  if (is.null(coords)) coords <- c(centroid.poly(pol), 0)
  if (is.null(norm)) norm <- c(0, 0, 1)
  if (is.null(aproj)) aproj <- 1.0
  
  A <- area.poly(pol) / aproj

  # Calculate the forces
  F <- -p * A * norm

  # Calculate the moments:

  r <- coords

  M <- c(r[2]*F[3] - r[3]*F[2],  r[3]*F[1] - r[1]*F[3],  r[1]*F[2] - r[2]*F[1])

  return(c(F, M))
    
  

  
}

  





  
  


               
     


  
  

