require(wutils)
clen <- 100
myrain <- col2rgb(cfd.colors(clen))
mygray <- seq(0.02, 0.96, len=clen)
getindex <- function(x){
  dr <- x[1] - myrain[1,]
  dg <- x[2] - myrain[2,]
  db <- x[3] - myrain[3,]
  dd <- abs(dr) + abs(dg) + abs(db)
  ii <- which.min(dd)
  if (dd[ii] > 20) return(0)
  return(ii)
}


                      
  
  

col2gray <- function(img){

  d <- dim(img)
  g <- matrix(NA, nr=d[1], nc=d[2])
  for (i in 1:d[1])
    for (j in 1:d[2])
      g[i,j] <- getindex(img[i,j,])
  return(g)
  apply(img, c(1,2), getindex)
}


convert <- function(img){

  cat("Calculating gray scale...\n")
  idx <- col2gray(img*255)

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


convertFile <- function(ipng, opng){

  img <- readPNG(ipng)
  img <- convert(img)

  writePNG(img, opng)

}



