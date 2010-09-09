
msgSimNao <- function(msg=''){
  repeat{
    ans <- toupper(readline(paste(msg, "(S/N): ")))
    if (ans == "S" || ans == "N") break
    cat("Responda 'S' ou 'N'!\n")
  }

  return(ifelse(ans=="S", TRUE, FALSE))
}

mymenu <- function(choices, title=""){
  n <- length(choices)
  choices <- join(1:n, ": ", choices)
  repeat{
    cat(title, "\n\n")
    cat(choices, sep='\n')
    cat("\n")
    ans <- readInteger("Escolha: ")
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
  ltrim(rtrim(s))

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


readInteger <- function(msg=''){
  repeat{
    ans <- readline(msg)
    if (isStrInt(ans)) return(as.integer(ans))
    cat("Entre com um inteiro!\n")
  }

}

readNumber <- function(msg=''){
  repeat{
    ans <- readline(msg)
    if (isStrNum(ans)) return(as.double(ans))
    cat("Entre com um número!\n")
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
      stop("Esta funcao tem", nnums, "argumentos!\n")
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



