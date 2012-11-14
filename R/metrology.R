
#' Calculates the k factor.
#'
#' Calculates the k factor, useful when calculating uncertainties.
#'
#' @param p Probability.
#' @param df Degrees of freedom.
#' @seealso \code{\link{qt}}
#' @examples
#' k = kfactor(df=5)
#' print(k)
#' @export
kfactor <- function(p=0.95, df=5){

  p2 <- (p+1)/2

  return(qt(p2, df))
}


