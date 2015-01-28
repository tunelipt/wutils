#' Color palette with colors varying from blue to red.
#'
#' Color palette with colors varying from blue to red.
#' Typical color scheme used in CFD visualizations.
#'
#' @param Number of levels.
#' @return Vector with n colors.
#' @seealso \code{\link{rainbow}}
#' @examples
#' x <- cfd.colors(10)
#' keyWindow(1:10, x)
#' @export
cfd.colors <- function(n) rev(rainbow(n, start=0, end=0.66667))

#' @rdname contourf
#' @export
fidContour <- function(x = seq(0, 1, length.out = nrow(z)),
                       y = seq(0, 1, length.out = ncol(z)),
                       z,
                       xlim = range(x, finite=TRUE),
                       ylim = range(y, finite=TRUE),
                       zlim = range(z, finite=TRUE),
                       levels = pretty(zlim, nlevels), nlevels = 20,
                       color.palette = cm.colors,
                       col = color.palette(length(levels) - 1),
                       plot.title, plot.axes, key.title, key.axes,
                       asp = NA, xaxs = "i", yaxs = "i", las = 1,
                       axes = TRUE, frame.plot = axes,
                       contour.lines=FALSE,
                       nclevels = 10, clevels = pretty(zlim, nclevels),
                       labels = NULL,
                       vfont=NULL,  labcex = 0.6, drawlabels = TRUE, method = "flattest",
                       ccol = par("fg"), lty = par("lty"), lwd = par("lwd"),
                       shapeFun=NULL, plot.key=TRUE, ...){


  if (missing(z)) {
    if (!missing(x)) {
      if (is.list(x)) {
        z <- x$z
        y <- x$y
        x <- x$x
      }
      else {
        z <- x
        x <- seq.int(0, 1, length.out = nrow(z))
      }
    }
    else stop("no 'z' matrix specified")
  }
  else if (is.list(x)) {
    y <- x$y
    x <- x$x
  }
  if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
    stop("increasing 'x' and 'y' values expected")
  mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
  on.exit(par(par.orig))
  w <- (3 + mar.orig[2L]) * par("csi") * 2.54
  if (plot.key){
    layout(matrix(c(2, 1), ncol = 2L), widths = c(1, lcm(w)))
    par(las = las)
    mar <- mar.orig
    mar[4L] <- mar[2L]
    mar[2L] <- 1
    par(mar = mar)
    plot.new()
    plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", 
                yaxs = "i")
    rect(0, levels[-length(levels)], 1, levels[-1L], col = col)
    if (missing(key.axes)) {
      if (axes) 
        axis(4)
    }
    else key.axes
    box()
    if (!missing(key.title)) 
      key.title
  }
  mar <- mar.orig
  mar[4L] <- 1
  par(mar = mar)
  plot.new()
  plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
  if (!is.matrix(z) || nrow(z) <= 1L || ncol(z) <= 1L) 
    stop("no proper 'z' matrix specified")
  if (!is.double(z)) 
    storage.mode(z) <- "double"
    .filled.contour(x, y, z, levels, col)
  if (missing(plot.axes)) {
    if (axes) {
      title(main = "", xlab = "", ylab = "")
      Axis(x, side = 1)
      Axis(y, side = 2)
    }
  }
  else plot.axes
  if (frame.plot) 
    box()
  if (missing(plot.title)) 
    title(...)
  else plot.title
  invisible()
  
   # Draw the contour lines:
  if (contour.lines){
    contour(x, y, z, nlevels=nclevels, levels=clevels, labels=labels,
            labcex=labcex, drawlabels=drawlabels, method=method,
            vfont=vfont, col=ccol, lty=lty, lwd=lwd, add=TRUE)
  }
  if (!is.null(shapeFun)) shapeFun()
}

#' Creates a function that draws a circle.
#'
#' Creates a function that draws a circle at given center with given diameter.
#'
#' @param xc X center of the circle.
#' @param yc Y center of the circle.
#' @param D Diameter of the circle.
#' @param col Color that should be used.
#' @param ... Arguments that are passed to \code{\link{polygon}}.
#' @seealso \code{\link{contourf}} \code{\link{polygon}}
#' @examples
#' plot.new()
#' plot.window(c(-1.5, 1.5), c(-1.5, 1.5), asp=1)
#' fun <- makeCircleDrawer(0, 0, 1, 'black')
#' fun()
#' @export
makeCircleDrawer <- function(xc, yc, D, col=rgb(0,0,0,0.5), ...)
  function(){
    R <- D/2
    theta <- seq(0, 358, by=2)*pi/180
    x <- xc + R*cos(theta)
    y <- yc + R*sin(theta)

    polygon(x, y, border=NA, col=col, ...)
  }



#' Draws filled contour and contour lines.
#'
#' Basically a combination of functions \code{\link{contour}} and \code{\link{filled.contour}}.
#'
#' @param x,y locations of grid lines at which the values in ‘z’ are
#'          measured.  These must be in ascending order.  By default,
#'          equally spaced values from 0 to 1 are used.  If ‘x’ is a
#'          ‘list’, its components ‘x$x’ and ‘x$y’ are used for ‘x’ and
#'          ‘y’, respectively. If the list has component ‘z’ this is used
#'          for ‘z’.
#' @param z  A matrix containing the values to be plotted (‘NA’s are
#'          allowed).  Note that ‘x’ can be used instead of ‘z’ for
#'          convenience.
#' @param xlim,ylim,zlim x-, y- and z-limits for the plot.
#' @param levels a set of levels which are used to partition the range of ‘z’.
#'          Must be *strictly* increasing (and finite).  Areas with ‘z’
#'          values between consecutive levels are painted with the same
#'          color.
#' @param nlevels if ‘levels’ is not specified, the range of ‘z’, values is
#'          divided into approximately this many levels.
#' @param color.palette a color palette function to be used to assign colors in
#'           the plot.
#' @param col an explicit set of colors to be used in the plot.  This
#'          argument overrides any palette function specification.  There
#'          should be one less color than levels
#' @param plot.title statements which add titles to the main plot.
#'
#' @param plot.axes statements which draw axes (and a ‘box’) on the main plot.
#'          This overrides the default axes.
#' @param key.title statements which add titles for the plot key.
#'
#' @param key.axes statements which draw axes on the plot key.  This overrides
#'          the default axis.
#' @param      asp the y/x aspect ratio, see ‘plot.window’.
#'
#' @param     xaxs the x axis style.  The default is to use internal labeling.
#' @param     yaxs the y axis style.  The default is to use internal labeling.
#'
#' @param      las the style of labeling to be used.  The default is to use
#'          horizontal labeling.
#' @param axes,frame.plot logicals indicating if axes and a box should be
#'          drawn, as in ‘plot.default’.
#' @param contour.lines Plot the contour lines?
#' @param nclevels number of contour levels desired *iff* ‘levels’ is not
#'          supplied.
#' @param   clevels numeric vector of levels at which to draw contour lines.
#' @param   labels a vector giving the labels for the contour lines.  If ‘NULL’
#'          then the levels are used as labels, otherwise this is coerced
#'          by ‘as.character’.
#' @param   labcex ‘cex’ for contour labelling.  This is an absolute size, not a
#'          multiple of ‘par("cex")’.
#' @param drawlabels logical.  Contours are labelled if ‘TRUE’.
#' @param   method character string specifying where the labels will be located.
#'          Possible values are ‘"simple"’, ‘"edge"’ and ‘"flattest"’
#'          (the default).  See the ‘Details’ section.
#' @param   vfont if ‘NULL’, the current font family and face are used for the
#'          contour labels.  If a character vector of length 2 then
#'          Hershey vector fonts are used for the contour labels. The
#'          first element of the vector selects a typeface and the second
#'          element selects a fontindex (see ‘text’ for more
#'          information).  The default is ‘NULL’ on graphics devices with
#'          high-quality rotation of text and ‘c("sans serif", "plain")’
#'          otherwise.
#' @param     ccol color for the lines drawn.
#' @param      lty line type for the lines drawn.
#' @param      lwd line width for the lines drawn.
#' @param      add logical. If ‘TRUE’, add to a current plot.
#' @param  shapeFun Function that plots something on the canvas.
#' @seealso \code{\link{contour}} \code{\link{filled.contour}}
#' @export
contourf <- function(x = seq(0, 1, length.out = nrow(z)),
                     y = seq(0, 1, length.out = ncol(z)),
                     z,
                     xlim = range(x, finite=TRUE),
                     ylim = range(y, finite=TRUE),
                     zlim = range(z, finite=TRUE),
                     levels = pretty(zlim, nlevels), nlevels = 20,
                     color.palette = cm.colors,
                     col = color.palette(length(levels) - 1),
                     plot.title, plot.axes, key.title, key.axes,
                     asp = NA, xaxs = "i", yaxs = "i", las = 1,
                     axes = TRUE, frame.plot = axes,
                     contour.lines=TRUE,
                     nclevels = 10, clevels = pretty(zlim, nclevels),
                     labels = NULL,
                     vfont=NULL,  labcex = 0.6, drawlabels = TRUE, method = "flattest",
                     ccol = par("fg"), lty = par("lty"), lwd = par("lwd"),
                     shapeFun=NULL, plot.key=TRUE, ...){


  if (missing(z)) {
    if (!missing(x)) {
      if (is.list(x)) {
        z <- x$z
        y <- x$y
        x <- x$x
      }
      else {
        z <- x
        x <- seq.int(0, 1, length.out = nrow(z))
      }
    }
    else stop("no 'z' matrix specified")
  }
  else if (is.list(x)) {
    y <- x$y
    x <- x$x
  }
  if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
    stop("increasing 'x' and 'y' values expected")
  mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
  on.exit(par(par.orig))
  w <- (3 + mar.orig[2L]) * par("csi") * 2.54
  if (plot.key){
    layout(matrix(c(2, 1), ncol = 2L), widths = c(1, lcm(w)))
    par(las = las)
    mar <- mar.orig
    mar[4L] <- mar[2L]
    mar[2L] <- 1
    par(mar = mar)
    plot.new()
    plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", 
                yaxs = "i")
    rect(0, levels[-length(levels)], 1, levels[-1L], col = col)
    if (missing(key.axes)) {
      if (axes) 
        axis(4)
    }
    else key.axes
    box()
    if (!missing(key.title)) 
      key.title
  }
  mar <- mar.orig
  mar[4L] <- 1
  par(mar = mar)
  plot.new()
  plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
  if (!is.matrix(z) || nrow(z) <= 1L || ncol(z) <= 1L) 
    stop("no proper 'z' matrix specified")
  if (!is.double(z)) 
    storage.mode(z) <- "double"
    .filled.contour(x, y, z, levels, col)
  if (missing(plot.axes)) {
    if (axes) {
      title(main = "", xlab = "", ylab = "")
      Axis(x, side = 1)
      Axis(y, side = 2)
    }
  }
  else plot.axes
  if (frame.plot) 
    box()
  if (missing(plot.title)) 
    title(...)
  else plot.title
  invisible()
  
   # Draw the contour lines:
  if (contour.lines){
    contour(x, y, z, nlevels=nclevels, levels=clevels, labels=labels,
            labcex=labcex, drawlabels=drawlabels, method=method,
            vfont=vfont, col=ccol, lty=lty, lwd=lwd, add=TRUE)
  }
  if (!is.null(shapeFun)) shapeFun()
}



