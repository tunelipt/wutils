require(gpclib)
require(deldir)

voronoi <- function(x,y, xb=NULL, yb=NULL){
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







  
  


               
     


  
  

