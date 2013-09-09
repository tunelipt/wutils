# Graphing utilities:


#' Generate layout for multiple figures.
#'
#' Calculates a multiple page layout for plotting multiplie figures.
#'
#' @param nfig Number of figures that will be plotted.
#' @param nrow Number of rows on each page.
#' @param ncol Number of columns on each page.
#' @param leg Add a common legend region?
#' @param byrow fill each page by rows?
#' @param leg.side Side to put common legend (1-bottom, 2-left, 3-top, 4-right).
#' @param Size of the common legend window.
#' @return A list containing the layout.
#' @seealso \code{\link{arrayLayout}} \code{\link{legendWindow}}
#' @examples
#' lay <- getArrayLayout(6, 2, 2)
#' arrayLayout(1, lay)
#' plot(1:10)
#' plot(1:10)
#' plot(1:10)
#' plot(1:10)
#' legendWindow("center", legend="Points", pch=1)
#' arrayLayout(2, lay)
#' plot(1:10)
#' plot(1:10)
#' legendWindow("center", legend="Points", pch=1)
#' @export
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


#' Creates the layout of a page.
#'
#' Uses the layout calculated by \code{\link{getArrayLayout}} to create
#' the layout of a page.
#'
#' @param page Page number to get layout.
#' @param lay Layout structure calculated with \code{\link{getArrayLayout}}.
#' @param Analogous to respect parameter in \code{\link{layout}} function.
#' @seealso \code{\link{getArrayLayout}} \code{\link{layout}}
#' @examples
#' lay <- getArrayLayout(6, 2, 2)
#' arrayLayout(1, lay)
#' plot(1:10)
#' plot(1:10)
#' plot(1:10)
#' plot(1:10)
#' legendWindow("center", legend="Points", pch=1)
#' arrayLayout(2, lay)
#' plot(1:10)
#' plot(1:10)
#' legendWindow("center", legend="Points", pch=1)
#' @export
arrayLayout <- function(page, lay, respect=FALSE){
  mat <- lay$matrix[,,page]
  dim(mat) <- dim(lay$matrix)[1:2]
  
  layout(mat, widths=lay$widths, heights=lay$heights, respect=respect)
}


#' Creates a legend on a window of its own.
#'
#' Simply a wrapper around \code{\link{legend}} to create a window that contains only
#' the legend.
#'
#' @param ... Parameters that will be passed on to \code{\link{legend}}.
#' @seealso \code{\link{legend}}
#' @examples
#' lay <- getArrayLayout(6, 2, 2)
#' arrayLayout(1, lay)
#' plot(1:10)
#' plot(1:10)
#' plot(1:10)
#' plot(1:10)
#' legendWindow("center", legend="Points", pch=1)
#' arrayLayout(2, lay)
#' plot(1:10)
#' plot(1:10)
#' legendWindow("center", legend="Points", pch=1)
#' @export
legendWindow <- function(...){

  plot.new()
  par.keep <- par(no.readonly=TRUE)
  on.exit(par(par.keep))
  par(mar=rep(0.1, 4))
  plot.window(c(0,1), c(0,1))
  
  legend(...)
}
  
  
#' Shows the layout of a single page.
#'
#' Shows the layout of a page calculated using
#' \code{\link{getArrayLayout}}. Basically an example of how
#' the functions \code{\link{getArrayLayout}}, \code{\link{arrayLayout}} and '
#' \code{\link{legend}}. It plots random data using the layout of a single page.
#'
#' @param lay Layout returned by \code{\link{getArrayLayout}}
#' @param page Page to be plotted.
#' @examples
#' lay <- getArrayLayout(6, 2, 2)
#' arrayLayout.show(lay, 1)
#' arrayLayout.show(lay, 2)
#' @export
arrayLayout.show <- function(lay, page){

  arrayLayout(page, lay)
  figs <- lay$fig.pos[,page]

  for (f in figs[!is.na(figs)]){
    plot(rnorm(100), main=f, ty='l')
    }
  if (lay$leg)
    legendWindow("center", "rnorm(100)", lty=1)
}


#' Generate a color interpolation function.
#'
#' Generates a function that calculates a color scale.
#'
#' In filled contour plots, a color corresponds to a value. This function
#' creates a new function that given a value, returns the corresponding color.
#' This function can use several different color palettes and can handle
#' values outside the values limits.
#'
#' @param xmin Minimum value of the color scale.
#' @param xmax Maximim value of the color scale.
#' @param n Number of intermediate colors.
#' @param palette Color palette used.
#' @param underflow Color to use for smaller values, first color if NULL.
#' @param underflow Color to use for largerr values, first color if NULL.
#' @return Function that returns a color given a numeric value.
#' @examples
#' fun <- makeColorFun(0, 1, underflow='black', overflow='white')
#' x <- c(-1, seq(0, 1, len=5), 2)
#' xcol <- fun(x)
#' keyWindow(x, xcol)
#' fun <- makeColorFun(0, 1)
#' xcol <- fun(x)
#' keyWindow(x, xcol)
#' @export 
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



#' Plots a color keymap.
#'
#' Plots a standalone keymap on its own window.
#'
#' @param levels Values that should be plotted.
#' @param colors Corresponding colors.
#' @param side On which side should the legend be plotted.
#' @param border Color of the border.
#' @param mar Size of the margins.
#' @param spc A parameter that contracts (spc < 1) or expands (z>1) the size of the keymap.
#' @param barw Width of the keymap.
#' @param ... Parameters passed to \code{\link{text}} function.
#' @seealso \code{\link{rect}} \code{\link{text}} \code{\link{contourf}}
#' @examples
#' keyWindow(1:10, side=4)
#' keyWindow(1:10, side=1)
#' keyWindow(1:10, side=4, spc=0.6)
#' keyWindow(1:10, side=4, barw=0.2)
#' @export
keyWindow <- function( levels, colors, side=4,
                      border=NULL, mar=rep(0.2, 4), spc=1, barw=0.5, ...){

  par.keep <- par(no.readonly=TRUE)
  on.exit(par(par.keep))

  
  if (!is.null(mar)) par(mar=mar)

  n <- length(levels)
  zl <- 0:(n-1)
  zu <- 1:n
  z <- (zl + zu)/2
  dz <- n/2*(1/spc - 1)
  zlim <- c(-dz, n+dz)
  plot.new()
  if (side==1){
    plot.window(xlim=zlim, ylim=c(0,1))
    rect(zl, 1-barw, zu, 1, col=colors, border=border)
    text(z, 1-barw, levels, pos=side, ...)
  }else if (side==2){
    plot.window(ylim=zlim, xlim=c(0,1))
    rect(1-barw, zl, 1, zu, col=colors, border=border)
    text(1-barw,z, levels, pos=side, ...)
  }else if (side==3){
    plot.window(xlim=zlim, ylim=c(0,1))
    rect(zl, 0, zu, barw, col=colors, border=border)
    text(z, barw, levels, pos=side, ...)
  }else{
    plot.window(ylim=zlim, xlim=c(0,1))
    rect(0, zl, barw, zu, col=colors, border=border)
    text(barw,z, levels, pos=side, ...)
  }

}
  

#' Generic graphics device creator
#'
#' An interface that creates graphical devices from a string characterizing the name.
#'
#' Very often different types of graphics need to be created. It is very annoying
#' to enter all the parameters one at a time. Given the name of the device type and the size,
#' this function returns a function that creates the appropriate device with the correct
#' file extension.
#'
#' The accepted devices are 'pdf', 'xfig', 'pictex', 'svg', 'wmf', 'postscript' (eps),
#' 'window', 'png', 'jpeg', 'bmp', 'tiff', 'X11', 'quartz'. Default is pdf. For bitmaps
#' the default resolution is 80 dpi.
#'
#' @param dev Character containing device name.
#' @param width Default width 
#' @param height Default height
#' @param ... Other parameters that will passed on to tghe device function.
#' @param dpi Dots per inche for use in bitmap devices.
#' @param pointsize Default size for plotted text in bitmap devices.
#' @examples
#' fun <- makeDevice('pdf')
#' fun('teste')  # Create teste.pdf file.
#' plot(1:10)
#' dev.off()
#' fun <- makeDevice('png')
#' fun('teste') # Create teste.png file.
#' plot(1:10)
#' dev.off()
#' @export
makeDevice <- function(dev='pdf', width=6, height=6, ..., dpi=96, pointsize=16){
  wb <- round(width * dpi)
  hb <- round(width * dpi)

  if (dev=='pdf')
    fun <- function(bname, width=width, height=height) pdf(paste(bname, '.pdf', sep=''),
                                          width=width, height=height, ...)
  else if (dev=='xfig') 
    fun <- function(bname, width=width, height=height) xfig(paste(bname, '.fig', sep=''),
                                          width=width, height=height, ...)
  else if (dev=='pictex')
    fun <- function(bname, width=width, height=height) pictex(paste(bname, '.tex', sep=''),
                                          width=width, height=height, ...)
  else if (dev=='svg')
    fun <- function(bname, width=width, height=height) svg(paste(bname, '.svg', sep=''),
                                          width=width, height=height, ...)
  else if (dev=='wmf')
    fun <- function(bname, width=width, height=height) win.metafile(paste(bname, '.wmf', sep=''),
                                          width=width, height=height, ...)
  else if (dev=='postscript')
    fun <- function(bname, width=width, height=height) postscript(paste(bname, '.eps', sep=''),
                                          width=width, height=height,  horizontal=FALSE, ...)
  else if (dev=='windows')
    fun <- function(bname="", width=width, height=height) windows(width=width, height=height, ...)
  else if (dev=='png')
    fun <- function(bname, width=width, height=height) png(paste(bname, '.png', sep=''),
                                       width=width*dpi, height=height*dpi, pointsize=pointsize, ...)
  else if (dev=='jpeg')
    fun <- function(bname, width=width, height=height) jpeg(paste(bname, '.jpg', sep=''),
                                       width=width*dpi, height=height*dpi, pointsize=pointsize, ...)
  else if (dev=='bmp')
    fun <- function(bname, width=width, height=height) bmp(paste(bname, '.bmp', sep=''),
                                       width=width*dpi, height=height*dpi, pointsize=pointsize, ...)
  else if (dev=='tiff')
    fun <- function(bname, width=width, height=height) tiff(paste(bname, '.tiff', sep=''),
                                       width=width*dpi, height=height*dpi, pointsize=pointsize, ...)
  else if (dev=='X11')
    fun <- function(display="", width=width, height=height) X11(display, width=width,
                                               height=height, ...)
  else if (dev=='quartz')
    fun <- function(title="", width=width, height=height) quartz(title, width=width,
                                             height=height, ...)
  else
    fun <- function(bname, width=width, height=width) pdf(paste(bname, '.pdf', sep=''),
                                          width=width, height=height, ...)

  return(fun)
}
  
    
