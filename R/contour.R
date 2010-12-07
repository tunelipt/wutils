
cfd.colors <- function(n) rev(rainbow(n, start=0, end=0.66667))

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
                       shapeFun=NULL, ...){


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
  mar <- mar.orig
  mar[4L] <- 1
  par(mar = mar)
  plot.new()
  plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
  if (!is.matrix(z) || nrow(z) <= 1L || ncol(z) <= 1L) 
    stop("no proper 'z' matrix specified")
  if (!is.double(z)) 
    storage.mode(z) <- "double"
  .Internal(filledcontour(as.double(x), as.double(y), z, as.double(levels), 
                          col = col))
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



makeCircleDrawer <- function(xc, yc, D, col=rgb(0,0,0,0.5))
  function(){
    R <- D/2
    theta <- seq(0, 358, by=2)*pi/180
    x <- xc + R*cos(theta)
    y <- yc + R*sin(theta)

    polygon(x, y, border=NA, col=col)
  }


