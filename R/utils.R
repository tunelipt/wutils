
msgYesNo <- function(msg='', pt=FALSE){
  if (pt){
    y<-'S'
    n<-'N'
    question <- "Responda 'S' ou 'N'!\n"
  }else{
    y <- 'Y'
    n <- 'N'
    question <- "Answer 'Y' or 'N'!\n"
  }
  msg2 <- paste("(", y, "/", n, "): ", sep='')
  repeat{
    ans <- toupper(readline(paste(msg, msg2)))
    if (ans == y || ans == n) break
    cat(question)
  }

  return(ifelse(ans==y, TRUE, FALSE))
}

mymenu <- function(choices, title="", pt=FALSE){
  if (pt)
    question <- "Escolha: "
  else
    question <- "Choice: "
  
  n <- length(choices)
  choices <- join(1:n, ": ", choices)
  repeat{
    cat(title, "\n\n")
    cat(choices, sep='\n')
    cat("\n")
    ans <- readInteger(question, pt)
    if (any(ans==1:n)) return(ans)
  }
}

rtrim <- function(s, chars=" \n\t\r"){
  pattern <- paste('[', chars, ']+$', sep='')
  sub(pattern, "", s)
}
ltrim <- function(s, chars=" \n\t\r"){
  pattern <- paste('^[', chars, ']+', sep='')
  sub(pattern, "", s)
}
trim <- function(s, chars=' \n\t\r')
  ltrim(rtrim(s, chars), chars)

isStrInt <- function(s){
  s <- trim(s)
  pattern <- "^[+-]?[0-9]+$"
  m <- regexpr(pattern, s)
  ifelse(m > 0, TRUE, FALSE)
}

isStrNum <- function(s){
  s <- trim(s)
  pattern <- "^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$"
  m <- regexpr(pattern, s)
  ifelse(m > 0, TRUE, FALSE)
}


readInteger <- function(msg='', pt=FALSE){
  if (pt)
    question <- "Entre com um inteiro!\n"
  else
    question <- "Integer expected!\n"
  
  repeat{
    ans <- readline(msg)
    if (isStrInt(ans)) return(as.integer(ans))
    cat(question)
  }

}

readNumber <- function(msg='', pt=FALSE){
  if (pt)
    question <- "Entre com um número!\n"
  else
    question <- "Number expected!\n"
  repeat{
    ans <- readline(msg)
    if (isStrNum(ans)) return(as.double(ans))
    cat(question)
  }
}

numString <- function(x, n=3)
  substring(paste(10^n + x), 2)

buildFileNameFun <- function(..., prefix="ponto", ext=".rda", sep='-'){
  nums <- list(...)
  nnums <- length(nums)
  nc <- nchar(paste(nums))
  function(...){
    x <- list(...)
    nx <- length(x)
    if (nx != nnums)
      stop("This function has", nnums, "arguments!\n")
    snums <- character(nnums)
    for (i in 1:nnums)
      snums[i] <- numString(x[[i]], nc[i])
    return(paste(prefix, sep, paste(snums, collapse=sep), ext, sep=''))
  }


}

buildFileName <- function(n, prefix='ponto', ext='.rda', sep='-', nc=NULL){
  if (is.null(nc)) nc <- nchar(max(n))
  paste(prefix, sep, numString(n, nc), ext, sep='')
}

join <- function(..., sep='', collapse=NULL)
  paste(..., sep=sep, collapse=collapse)

basename2 <- function(path, ext=NULL){
  path <- base::basename(path)
  if (is.null(ext))
    return(path)
  sub(join(ext, '$'), '', path)
}


#' Sort strings with numbers using the numbers as sorting elements.
#'
#' Often a sequence of strings contain a numbers and it is necessary
#' to sort these strings according to these numbers and not alphabetically.
#'
#' @param x Strings to be sorted containing at least one integer group.
#' @param decreasing Sort the sequence in decreasing order?
#' @param index.return Return the indexes of the sorted vector?
#' @return Sorted vector or, depending on argument \code{index.return} index.
#' @export
sortNum <- function(x, decreasing=FALSE, index.return=FALSE){

  m <- regexpr('[0-9]+([^0-9]+)?$', x)
  
  ms <- as.integer(m)
  xx <- substring(x, ms)
  m <- regexpr('[0-9]+', xx)
  ml <- attr(m, 'match.length')
  
  xx <- as.integer(substr(xx, 1, ml))

  ix <- sort(xx, decreasing=decreasing, index.return=TRUE)$ix


  if (index.return)
    return(list(x=x[ix], ix=ix))
  else
    return(x[ix])
}


