# Subroutines that handle table creation for LaTeX output.


#' Calculates multiple page/column table layout.
#'
#' Given a table with a certain number of lines, this function calculates the number
#' of columns and pages necessary to print the table in paper.
#'
#' Data frames are very useful and there are several ways to generate an output.
#' Often, however, the tables should be printed and the printout spans several pages
#' and what is worse, if the table has few columns, there will be lots of white space.
#' This function calculates a layout that can be used to pack the table in several pages.
#' The printed table might use several pages and several columns on each page. On
#' each page, the columns bight be filled by rows or by columns and the last page,
#' usually partially filled only, how should it be filled?
#'
#' @param nlines Number of lines of the original table.
#' @param ntabcols Number of coumns the page should be subdivided.
#' @param ntablines Number of lines each page should have.
#' @param byrow Should the tables be filled by rows?
#' @param fillbyrow How to handle partial last pages. Should the last page be "condensed" to minimize the number of rows?
#' @return 3D array where the indicies specify the line, column and page where each line will go.
#' @examples
#' print(tableLayout(30, 2, 9))
#' print(tableLayout(30, 2, 9), byrow=TRUE)
#' print(tableLayout(30, 2, 9, filledbyrow=TRUE))
#' print(tableLayout(30, 3, 10))
#' @export
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

  
    

#' Generate a LaTeX table from the table layout.
#'
#' After generating a layout with \code{\link{tableLayout}} this function generates
#' a set of LaTeX files that contain the data in table format.
#'
#' @param suffix Suffix of the file name.
#' @param tab Data to be written to LaTeX files.
#' @param tab.lay Table layout computed with \code{\link{tableLayout}}.
#' @param Header of each column. If not specified, the names of each column is used.
#' @param units An optional second line in the header specifying units or some other info.
#' @param pos LaTeX tabular position specifier for each variable.
#' @param dname Directory name where the file should be written.
#' @param preamble Some message that should be written before the header.
#' @export
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
    if (!is.null(preamble)) wline2(con, preamble)
    wline2(con, paste(header, collapse=' & '))
    if (!is.null(units)) wline2(con, paste(rep(units, ncols), collapse=' & '))
    hline(con, 2)
    write.table(buildTable(tab, tab.lay[,,i]), file=con, quote=FALSE, sep=' & ', eol='\\\\\n',
                row.names=FALSE, col.names=FALSE)
    hline(con, 2)
    wline(con, '\\end{tabular}')
    close(con)
  }
  
}
  
  
    
