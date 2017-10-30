
#' Hilbert transform
#' Calculates the Hilbert transform of a vector or columns of a matrix
#'
#' @param x Vector or matrix
#' @return Hilbert transform of x
#' @examples
#' x <- seq(0, 1, by=0.001)[1:1000]
#' y <- sin(6*pi*x)
#' H <- hilbert(x)
#' plot(y, ty='l', ylim=range(y, Re(H), Im(H)))
#' lines(abs(H), col=2, lwd=2)
#' lines(Re(H), col=3)
#' lines(Im(H), col=4)
#' @export
hilbert <- function(x){

  d <- attributes(x)

  x <- as.matrix(x)
  nc <- ncol(x)
  nr <- nrow(x)
  
  X <- mvfft(x)
  Y <- 0*X
  if (nr %% 2 == 0){
    n2 <- nr / 2
    Y[1,] <- X[1,]
    Y[2:n2,] <- 2*X[2:n2,]
  }else{
    Y[1,] <- X[1,]
    n2 <- (nr+1)/2
    Y[2:n2,] <- 2*X[2:n2,]
  }

  y <- (1/nr * (mvfft(Y, TRUE)))
  attributes(y) <- d
  return(y)
}



   
