require(gpclib)
require(deldir)

voronoi <- function(x,y, xb=NULL, yb=NULL){
  if (is.null(xb))
    xr <- range(x)
  else
    xr <- range(xb)
  if (is.null(yb))
    yr <- range(y)
  else
    yr <- range(yb)
  
  dx <- xr[2]-xr[1]
  dy <- yr[2]-yr[1]
  
  tri <- deldir(x,y, rw=c(xr[1]-dx, xr[2]+dx, yr[1]-dy, yr[2]+dy))

  
  return(tile.list(tri))

}


voronoi2polygonlst <- function(vor){

  return(lapply(vor, function(v) as(cbind(v$x, v$y), 'gpc.poly')))
}


splitWith <- function(vor, hull){

  return(lapply(vor, function(v) intersect(v, hull)))
}







  
  


               
     


  
  

