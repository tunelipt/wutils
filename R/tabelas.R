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
#' @seealso \code{\link{latexOutput}}
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
#' @param hl Put a horizontal line every hl lines for readability. If NULL, don't put the lines.
#' @param postinfo Additional lines at the end of the table.
#' @return Name of the .tex files generated.
#' @seealso \code{\link{tableLayout}}
#' @export
latexOutput <- function(suffix, tab, tab.lay,  header=NULL, units=NULL,
                         pos=NULL, dname=NULL, preamble=NULL, hl=NULL, postinfo=NULL){

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
  ff <- character(npages)
  
  for (i in 1:npages){
    fname <- file.path(dname, paste(suffix, '-', i, '.tex', sep=''))
    ff[i] <- fname
    con <- file(fname, open='w')
    wline(con, '\\begin{tabular}{', pos, '}')
    hline(con, 2)
    if (!is.null(preamble)) wline2(con, preamble)
    wline2(con, paste(header, collapse=' & '))
    if (!is.null(units)) wline2(con, paste(rep(units, ncols), collapse=' & '))
    hline(con, 2)
    xtab <- buildTable(tab, tab.lay[,,i])
    if (is.null(hl))
      write.table(xtab, file=con, quote=FALSE, sep=' & ', eol='\\\\\n',
                  row.names=FALSE, col.names=FALSE)
    else{
      ntl <- dim(xtab)[1]
      istart <- 1
      while(TRUE){
        iend <- istart + hl - 1
        if (iend < ntl){
          write.table(xtab[istart:iend,, drop=FALSE], file=con, quote=FALSE, sep=' & ', eol='\\\\\n',
                      row.names=FALSE, col.names=FALSE)
          hline(con,1)
          istart <- iend+1
        }else{
          write.table(xtab[istart:ntl,, drop=FALSE], file=con, quote=FALSE, sep=' & ', eol='\\\\\n',
                      row.names=FALSE, col.names=FALSE)
          break
        }
      }
    }
    hline(con, 2)
    if (!is.null(postinfo)){
      wline2(con, postinfo)
      hline(con, 2)
    }
    wline(con, '\\end{tabular}')
    close(con)
  }

  return(ff)
}
  
  

#' Multiple page table generation.
#'
#' Function that automates multiple page table creation.
#'
#' When writing scientific reports, often it is necessary to write very long tables
#' that span several pages. This function wraps the tables created with \code{\link{latexOutput}}
#' with a LaTeX table environment, including captions and labels. In the report,
#' instead putting the each individual table, this function will generate an file that
#' can be included directly.
#'
#' @param fname Name of the LaTeX include file to generate.
#' @param ftab Array with individual table names.
#' @param caption Caption to be used in each figure. At the end, Part i/n is included.
#' @param lab.prefix Label prefix. If NULL, the labels will be tab:ftab[i]-i.
#' @param center Center the tabular environment?
#' @param pos LaTeX float positioning parameters.
#' @param charsize Different character size to use - "footnotesize" for example.
#' @export
latexTable <- function(fname, ftab, caption, lab.prefix=NULL, center=TRUE, pos=NULL, charsize=NULL){

  ntab <- length(ftab)
  con <- file(fname, open='w')
  for (i in 1:ntab){
    cat("\\begin{table}", file=con)
    if (!is.null(pos))
      cat(pos, "\n", file=con)
    else
      cat("\n", file=con)
      
    cat("\\caption{", caption, " Parte ", i, "/", ntab, ".}\n", file=con, sep='')
    if (center) cat("\\centering\n", file=con)
    
    xf <- basename2(ftab[i], '.tex')

    if (!is.null(charsize)) cat("{\\", charsize, "\n", file=con, sep='')

    tablines <- readLines(ftab[i])
    writeLines(tablines, con=con)
    
    if (!is.null(charsize)) cat("}\n", file=con, sep='')

    if (is.null(lab.prefix))
      cat("\\label{tab:", xf, "}\n", file=con, sep='')
    else
      cat("\\label{", lab.prefix, '-', i, "}\n", file=con, sep='')
    cat('\\end{table}\n\n\n', file=con)
  }

  close(con)
}
