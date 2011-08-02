getTri <- function(stri){
  nl <- length(stri)
  
  con <- textConnection(stri)
  on.exit(close(con))
  x <- scan(con, quiet=TRUE)
  dim(x) <- c(3, 3, nl)
  
  return(x)

}  
  
joinTri <- function(tri){

  if (!is.list(tri)) tri <- list(tri)

  n <- length(tri)
  ntri <- 0
  for (i in 1:n) ntri <- ntri + dim(tri[[i]])[3]

  x <- array(dim=c(3, 3, ntri))

  count <- 1
  for (i in 1:n)
    for (k in 1:dim(tri[[i]])[3]){
      x[,,count] <- tri[[i]][,,k]
      count <- count + 1
    }

  return(x)
}  

# Reads a raw file containing surfaces made of triangles.
readRaw <- function(fname){

  s <- readLines(fname)

  header <- grep("Object[0-9]+", s)

  ntri <- length(header)

  lstart <- header+1
  lend <- c((header-1)[2:ntri], length(s))
  #return(list(s, lstart, lend))
  
  tri <- lapply(1:ntri, function(i) getTri(s[lstart[i]:lend[i]]))
  return(tri)
}
  
# Converts a raw mesh to vtk old style file.
raw2vtk <- function(tri, fname){
  tri <- joinTri(tri)

  d <- dim(tri)
  n <- d[3]

  con <- file(fname, open='w')
  cat('# vtk DataFile Version 2.0\n Triangles\nASCII\nDATASET UNSTRUCTURED_GRID\nPOINTS', n*3, 'float\n', file=con)
  for (i in 1:n)
    for (k in 1:3)
      cat(tri[,k,i], "\n", file=con)
  cat("\nCELLS", n, n*4, "\n", file=con)
  count <- 0
  for (i in 1:n){
    cat(3, count:(count+2), "\n", file=con)
    count <- count + 3
  }
  
  cat("\nCELL_TYPES", n, "\n", file=con)
  for (i in 1:n)
    cat(5, "\n", file=con)
  close(con)
  
  
}
  
