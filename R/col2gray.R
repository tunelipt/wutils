require(png) 

#' Converts an image from a color keymap to grey scale.
#'
#' An utility function used when converting some figure that has
#' a color map from a color scale to a grey scale.
#'
#' If a pixel has o color too far away from the color scale, the pixel is left
#' without modification since it probably belongs to annotations on the figure.
#'
#' @param img Array containing the image.
#' @param tol Tolerance in color interpolation.
#' @param clen Number of color subdivisions that should be used.
#' @param myrain Color scale.
#' @return Index characterizing the level of each pixel.
#' @export
col2gray <- function(img, tol=20, clen=100, myrain=col2rgb(cfd.colors(clen))){


  getindex <- function(x){
    dr <- x[1] - myrain[1,]
    dg <- x[2] - myrain[2,]
    db <- x[3] - myrain[3,]
    dd <- abs(dr) + abs(dg) + abs(db)
    ii <- which.min(dd)
    if (dd[ii] > tol) return(0)
    return(ii)
  }


  d <- dim(img)
  g <- matrix(NA, nr=d[1], nc=d[2])
  for (i in 1:d[1])
    for (j in 1:d[2])
      g[i,j] <- getindex(img[i,j,])
  return(g)
  apply(img, c(1,2), getindex)
}

#' Converts an image from a color keymap to grey scale.
#'
#' An utility function used when converting some figure that has
#' a color map from a color scale to a grey scale.
#'
#' If a pixel has o color too far away from the color scale, the pixel is left
#' without modification since it probably belongs to annotations on the figure.
#'
#' @param img Array containing the image.
#' @param tol Tolerance in color interpolation.
#' @param clen Number of color subdivisions that should be used.
#' @param myrain Color scale.
#' @param mygray Gray scale that is used when generating output.
#' @return Gray scale image.
#' @export
convert <- function(img, tol=20, clen=100, myrain=col2rgb(cfd.colors(clen)),
                    mygray=seq(0.02, 0.96, len=clen)){

  cat("Calculating gray scale...\n")
  idx <- col2gray(img*255,tol, clen,  myrain)

  cat("Generating new image...\n")

  ip0 <- idx>0
  r <- img[,,1]
  g <- img[,,2]
  b <- img[,,3]
  d <- dim(img)[1:2]
  dim(r) <- NULL
  dim(b) <- NULL
  dim(g) <- NULL
  dim(ip0) <- NULL
  gr <- mygray[idx[ip0]]
  r[ip0] <- gr
  g[ip0] <- gr
  b[ip0] <- gr
  dim(r) <- d
  dim(g) <- d
  dim(b) <- d
  
        

  img[,,1] <- r
  img[,,2] <- g
  img[,,3] <- b

  return(img)

}


#' Converts an image from a color keymap to grey scale.
#'
#' An utility function used when converting some figure that has
#' a color map from a color scale to a grey scale.
#'
#' If a pixel has o color too far away from the color scale, the pixel is left
#' without modification since it probably belongs to annotations on the figure.
#'
#' @param ipng Input png file. 
#' @param opng Output png file. 
#' @param tol Tolerance in color interpolation.
#' @param clen Number of color subdivisions that should be used.
#' @param color.palette Color scale.
#' @param glim Gray scale that is used when generating output.
#' @export
convertFile <- function(ipng, opng, tol=20, clen=100, color.palette=cfd.colors,
                        glim=c(0.02, 0.96)){

  myrain <- col2rgb(color.palette(clen))
  mygray <- seq(min(glim), max(glim), len=clen)

  img <- readPNG(ipng)
  img <- convert(img, tol, clen, myrain, mygray)

  writePNG(img, opng)

}



