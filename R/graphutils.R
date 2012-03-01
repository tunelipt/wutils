# Graphing utilities:



getArrayLayout <- function(nfig, nrow=4, ncol=3, leg=TRUE, byrow=FALSE, leg.side=1, leg.len=0.2){

  nperpage <- ncol*nrow
  npages <- ifelse( nfig %% nperpage, nfig %/% nperpage + 1, nfig %/% nperpage)

  mat <- matrix(1:nperpage, ncol=ncol, nrow=nrow, byrow=byrow)
  
  if (leg){
    if (leg.side==1){
      mat <- rbind(mat, nperpage+1)
      widths=rep(1, ncol)
      heights <- c(rep( (1-leg.len)/ nrow, nrow), leg.len)
    }else if (leg.side==2){
      mat <- cbind(nperpage+1, mat)
      heights <- rep(1, nrow)
      widths <- c(leg.len, rep( (1-leg.len)/ ncol, ncol))
    }else if (leg.side==3){
      mat <- rbind(nperpage+1, mat)
      widths=rep(1, ncol)
      heights <- c(rep(leg.len,  (1-leg.len)/ nrow, nrow))
    }else{
      mat <- cbind(mat, nperpage+1)
      heights <- rep(1, nrow)
      widths <- c(rep( (1-leg.len)/ ncol, ncol), leg.len)
    }
  }else{
    widths <- rep(1, ncol)
    heights <- rep(1, nrow)
  }
  
  layout.matrix <- array(NA, dim=c(dim(mat), npages))

  dd <- nfig %/% nperpage

  if (dd)
    for (i in 1:dd)
      layout.matrix[,,i] <- mat
  if (nfig %% nperpage){
    mat2 <- mat
    mat2[mat2==nperpage+1] <- -1
    nlast <- nfig - (nperpage*(npages-1))
    mat2[mat2>nlast] <- 0
    mat2[mat2==-1] <- nlast+1
    layout.matrix[,,npages] <- mat2
  }
  
  
  fig.pos <- matrix(NA, nrow=nperpage, ncol=npages, byrow=FALSE)
  fig.pos[1:nfig] <- 1:nfig
  return(list(matrix=layout.matrix, npages=npages, fig.pos=fig.pos, heights=heights, widths=widths,
              leg=leg, leg.side=leg.side))

  

}


arrayLayout <- function(page, lay, respect=FALSE)
  layout(lay$matrix[,,page], widths=lay$widths, heights=lay$heights, respect=respect)

legendWindow <- function(...){

  plot.new()
  par.keep <- par(no.readonly=TRUE)
  on.exit(par(par.keep))
  par(mar=rep(0.1, 4))
  plot.window(c(0,1), c(0,1))
  
  legend(...)
}
  
  

arrayLayout.show <- function(lay, page){

  arrayLayout(page, lay)
  figs <- lay$fig.pos[,page]

  for (f in figs[!is.na(figs)]){
    plot(rnorm(100), main=f, ty='l')
    }
  if (lay$leg)
    legendWindow("center", "rnorm(100)", lty=1)
}


makeColorFun <- function(xmin, xmax, n=100, palette=cfd.colors, underflow=NULL, overflow=NULL){
  
  cols <- palette(n)

  if (is.null(underflow)) underflow = cols[1]
  if (is.null(overflow)) overflow <- cols[n]

  xx <- seq(xmin, xmax, len=n)

  interp.fun <- approxfun(xx, 1:n)

  fun <- function(x){
    imin <- x<xmin
    imax <- x>xmax
    iother <- !imin & ! imax
    xcol <- character(length(x))
    dim(xcol) <- dim(x)
    
    xcol[imin] <- underflow
    xcol[imax] <- overflow
    xcol[iother] <- cols[round(interp.fun(x[iother]))]

    return(xcol)
  }

  return(fun)
}




keyWindow <- function( levels, colors, side=4,
                      border=NULL, mar=rep(0.2, 4), ...){

  par.keep <- par(no.readonly=TRUE)
  on.exit(par(par.keep))
  
  if (!is.null(mar)) par(mar=mar)

  n <- length(levels)
  zl <- 0:(n-1)
  zu <- 1:n
  z <- (zl + zu)/2

  plot.new()
  if (side==1){
    plot.window(xlim=c(0,n), ylim=c(0,2))
    rect(zl, 1, zu, 2, col=colors, border=border)
    text(z, 1, levels, pos=side, ...)
  }else if (side==2){
    plot.window(ylim=c(0,n), xlim=c(0,2))
    rect(1, zl, 2, zu, col=colors, border=border)
    text(1,z, levels, pos=side, ...)
  }else if (side==3){
    plot.window(xlim=c(0,n), ylim=c(0,2))
    rect(zl, 0, zu, 1, col=colors, border=border)
    text(z, 1, levels, pos=side, ...)
  }else{
    plot.window(ylim=c(0,n), xlim=c(0,2))
    rect(0, zl, 1, zu, col=colors, border=border)
    text(1,z, levels, pos=side, ...)
  }

}
  
