findMax <- function(dt){
  d <- dim(dt)
  nx <- d[1]
  nz <- d[2]
  
  colmax <- apply(dt, 1, max)
  ix <- which.max(colmax)
  rowmax <- apply(dt, 2, max)
  iz <- which.max(rowmax)
  return(c(ix, iz))
}


fidHelicopter <- function(x, z, dt, D){

  
  nx <- length(x)
  y <- z
  ny <- length(y)
  xx <- rep(x, ny)
  yy <- rep(y, each=nx)
  R <- D/2
  n <- nx*ny

  imax <- findMax(dt)
  ix <- imax[1]
  iy <- imax[2]

  xc <- x[ix]
  yc <- y[iy] 
  
  #prect <- as(cbind( c(min(x), max(x), max(x), min(x)), c(min(y), min(y), max(y), max(y))),
  #            "gpc.poly")

  theta <- seq(0, 358, by=2) * pi/180

  pcirc <- as(cbind(xc + R*cos(theta), yc + R*sin(theta)), "gpc.poly")

  vor <- voronoi2polygonlst(voronoi(xx, yy))
  
  region <- pcirc #intersect(prect, pcirc)

  inter <- splitWith(vor, region)
  return(inter)
}

weighedMean <- function(dt, plst){
  dim(dt) <- NULL

  area <- sapply(plst, area.poly)
  at <- sum(area)
  dtm <- sum(dt*area) / at
  return(dtm)
}


mean2 <- function(x, z, dt, D){

  nx <- length(x)
  nz <- length(z)

  imax <- findMax(dt)
  ix <- imax[1]
  iz <- imax[2]
  xc <- x[ix]
  zc <- z[iz]

  
  xx <- rep(x, nz)
  zz <- rep(z, each=nx)

  R <- D/2
  dist <- sqrt((xx-xc)**2 + (zz-zc)**2)
  dim(dt) <- NULL
  pts <- dist < R
  return(mean(dt[pts]))
}


  
interpolateFid <- function(x, z, dt, dx=1, dz=1, interpfun=approx){

  nx <- length(x)
  nz <- length(z)

  # Interpolar as linhas com x constante
  z1 <- seq(min(z), max(z), by=dx)
  x1 <- seq(min(x), max(x), by=dz)
  nz1 <- length(z1)
  nx1 <- length(x1)
  dt1 <- matrix(NA, nrow=nx, ncol=nz1)

  for (i in 1:nx)
    dt1[i,] <- interpfun(z, dt[i,], z1)$y
  

  dt2 <- matrix(NA, nrow=nx1, ncol=nz1)
  for (i in 1:nz1)
    dt2[,i] <- interpfun(x, dt1[,i], x1)$y
  
  return(list(x=x1, z=z1, dt=dt2))

}
  

dtTable <- function(fname, x, z, dt, dec=','){

  imax <- findMax(dt)
  xmax <- imax[1]
  zmax <- imax[2]
 
  nx <- length(x)
  nz <- length(z)

  dt <- formatC(round(t(dt), 1), decimal.mark=dec)

  dt[zmax,xmax] <- paste('\\textbf{', dt[zmax,xmax], '}', sep='')
  dt <- cbind(paste(z), dt)
 
  f <- file(fname, open='w')
  ccc <- paste('c', '|', paste(rep('c', nx), collapse=''), sep='')
  cat('\\begin{tabular}{', ccc,'}\n', sep='', file=f)
  # Escrever o cabeçalho:
  cat('\\hline\\hline\n', file=f)
  cat('& ', paste(x, collapse=' & '), '\\\\\n', sep='', file=f)
  cat('\\hline\\hline\n', file=f)
  write.table(dt, file=f, quote=FALSE, sep=' & ', eol='\\\\\n\\hline\n',
col.names=FALSE, row.names=FALSE)
  cat('\\hline\\hline\n', file=f)
  cat("\\end{tabular}\n", file=f)
  close(f)
}


