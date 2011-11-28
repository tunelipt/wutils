# Subroutines that handle table creation for LaTeX output.


tableLayout <- function(nlines, ntabcols, ntablines, byrow=FALSE, fillbyrow=TRUE){

  nperpage <- ntabcols * ntablines
  npages <- nlines %/% nperpage 

  
  partialpage <- (nlines %% nperpage ) > 0
  if (partialpage){
    npages <- npages + 1
  }
  npossible <- npages * nperpage
  lineMat <- array(0, dim=c(ntablines, ntabcols, npages))

  fullpages <- rep(TRUE, npages)
  if (partialpage) fullpages[npages] <- FALSE
  
  for (i in (1:npages)[fullpages])
      lineMat[,,i] <- matrix( (1:nperpage) + (i-1)*nperpage, nr=ntablines, nc=ntabcols, byrow=byrow)

  if (partialpage){

    x <- ((npages-1)*nperpage + 1):nlines
    n <- length(x)

    if (fillbyrow){
      nr <- ceiling(n / ntabcols)
      xx <- integer(nr*ntabcols)
      xx[1:n] <- x
      lineMat[1:nr,,npages] <- matrix(xx, nr=nr, nc=ntabcols, byrow=byrow)
    }else{

      nc <- ceiling(n / ntablines)
      xx <- integer(nc*ntablines)
      xx[1:n] <- x
      lineMat[,1:nc,npages] <- matrix(xx, nr=ntablines, nc=nc, byrow=byrow)
    }
  }
  return(lineMat)
}

  
    

latexOutput <- function(suffix, tab, tab.lay,  header=NULL, units=NULL,
                         pos=NULL, dname=NULL, preamble=NULL){

  npages <- dim(tab.lay)[3]
  
  ncols <-  dim(tab.lay)[2]
  nrows <-  dim(tab.lay)[1]

  nc <- dim(tab)[2]
  nr <- dim(tab)[1]

  if (is.null(dname))
    dname <- '.'
  else
    if (!file.exists(dname)) dir.create(dname)

  if (is.null(pos)) pos <- paste(rep('c', nc), collapse='', sep='')

  if (ncols > 1)
    pos <- paste(rep(pos,ncols), collapse='||')
  wline <- function(con, ...) cat(..., '\n', file=con, sep='')
  wline2 <- function(con, ...) cat(..., '\\\\\n', file=con, sep='')
  hline <- function(con, n=1) cat(rep('\\hline\n', n), file=con)

  if (is.null(header)) header=colnames(tab)

  header <- rep(header, ncols)
  buildTable <- function(tab, page){
    d <- dim(page)
    if (is.null(d)) dim(page) <- c(length(page), 1)
    d <- dim(page)
    nr <- d[1]
    nc <- d[2]
    ra <- nr
    ca <- nc

    ncol <- dim(tab)[2]

    for (i in 1:nr)
      if (all(page[i,]==0)){
        ra <- i-1
        break
      }
    
    pag <- matrix("", ra, nc*ncol)

    for (i in 1:ra)
      for (k in 1:nc){
        if (page[i,k] > 0) pag[i, (ncol*(k-1)+1):(ncol*k)] <- tab[page[i,k],]
      }
    return(pag)
    
  }
  
  for (i in 1:npages){
    fname <- file.path(dname, paste(suffix, '-', i, '.tex', sep=''))
    con <- file(fname, open='w')
    wline(con, '\\begin{tabular}{', pos, '}')
    hline(con, 2)
    if (!is.null(preamble)) wline(con, preamble)
    wline2(con, paste(header, collapse=' & '))
    if (!is.null(units)) wline2(con, paste(rep(units, 2), collapse=' & '))
    hline(con, 2)
    write.table(buildTable(tab, tab.lay[,,i]), file=con, quote=FALSE, sep=' & ', eol='\\\\\n',
                row.names=FALSE, col.names=FALSE)
    hline(con, 2)
    wline(con, '\\end{tabular}')
    close(con)
  }
  
}
  
  
    
