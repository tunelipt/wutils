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


