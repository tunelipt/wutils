
kfactor <- function(p, df=5){

  p2 <- (p+1)/2

  return(qt(p2, df))
}


