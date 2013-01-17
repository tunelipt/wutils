#' Displays a Yes/No prompt.
#'
#' Simple function for interactive input.
#' This function displays a prompt that await a Yes/No reply.
#' If the optional argument \code{pt} is TRUE, the function
#' expects S /N (portuguese).
#'
#' @param msh Prompt to be displayed before asking Y/N.
#' @param pt Use portuguese?
#' @return TRUE (YES), FALSE (NO).
#' @examples
#' ans <- msgYesNo("Sim ou Não", pt=TRUE)
#' print(ans)
#' ans <- msgYesNo("Yes or No")
#' print(ans)
#' @export
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
#' Simple menu input.
#'
#' Implements a simple menu were user can choose one of
#' several options.
#' @param choices Menu options.
#' @param title Title to be displayed in the menu.
#' @param pt Use portuguese on prompt?
#' @return Index of the chose option.
#' @seealso \code{\link{msgYesNo}}
#' @examples
#' ans <- mymenu(c("One", "Two", "Three"), "Select an option")
#' print(ans)
#' @export
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

#' String trimming.
#'
#' Trims whitespace off the ends of strings.
#'
#' \code{rtrim} trims the right end of the string,
#' \code{ltrim} trims the left end of the string and
#' \code{trim} trims both ends of the string.
#'
#' @param s Character array to be trimmed.
#' @param chars Characters to be trimmed.
#' @return Trimmed version of the character array.
#' @seealso \code{\link{sub}}
#' @examples
#' s <- "   garbagexxxx"
#' print(rtrim(s))
#' print(rtrim(s, 'x'))
#' print(ltrim(s))
#' print(ltrim(s, 'x'))
#' print(trim(s))
#' print(trim(s,'x'))
#' print(trim(s,' x'))
#' @aliases rtrim ltrim trim
#' @export
rtrim <- function(s, chars=" \n\t\r"){
  pattern <- paste('[', chars, ']+$', sep='')
  sub(pattern, "", s)
}

#' @rdname rtrim
#' @export
ltrim <- function(s, chars=" \n\t\r"){
  pattern <- paste('^[', chars, ']+', sep='')
  sub(pattern, "", s)
}


#' @rdname rtrim
#' @export
trim <- function(s, chars=' \n\t\r')
  ltrim(rtrim(s, chars), chars)


#' Is the string a number?
#'
#' Determines whether a string is a number (or integer)?.
#'
#'  \code{isStrInt} determines whether a string is a integer. 
#'  \code{isStrNum} determines whether a string is a number.
#'
#' @param s Character array.
#' @param dec Decimal point separator.
#' @return TRUE if input is a integer/number FALSE otherwise.
#' @examples
#' s <- "XYZ"
#' print(isStrInt(s))
#' print(isStrNum(s))
#' s <- "-123"
#' print(isStrInt(s))
#' print(isStrNum(s))
#' s <- "-123.234"
#' print(isStrInt(s))
#' print(isStrNum(s))
#' s <- "1e4"
#' print(isStrInt(s))
#' print(isStrNum(s))
#' @aliases isStrInt isStrNum
#' @export
isStrInt <- function(s){
  s <- trim(s)
  pattern <- "^[+-]?[0-9]+$"
  m <- regexpr(pattern, s)
  ifelse(m > 0, TRUE, FALSE)
}

#' @rdname isStrInt
#' @export
isStrNum <- function(s, dec='.'){
  s <- trim(s)
  pdec <- ifelse(dec=='.', '\\.', ',')
  pattern <- paste('^[-+]?[0-9]*', pdec, '?[0-9]+([eE][-+]?[0-9]+)?$', sep='')
  #pattern <- "^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$"
  m <- regexpr(pattern, s)
  ifelse(m > 0, TRUE, FALSE)
}

#' Reads numbers from prompt.
#'
#' Reads number from a prompt.
#'
#'  These functions print a prompt and repeats until the correct type of
#'  input is read and returns the input. The comments and messages can be
#'  displayed in portuguese if option \code{pt} is set to \code{TRUE}.
#'
#'  \code{readInteger} expects an integer from the user and
#'  \code{readNumber} a number (float or integer).
#'
#' @param msg Prompt to be displayed.
#' @param pt Use portuguese messages?
#' @return Number/Integer read.
#' @examples
#' print(readInteger("Enter an integer: "))
#' print(readNumber("Enter a number: "))
#' print(readInteger("Entre com um inteiro: ", pt=TRUE))
#' print(readNumber("Entre com um numero: ", pt=FALSE))
#' @export
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

#' @rdname readInteger
#' @export
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


#' Integer to string converter
#'
#' Converts an integer into a string with a given digits.
#' If necessary, the function prepends the number with zeros to fill the digits.
#'
#' When creating numbered file names, the file system lists the files alphabetically.
#' This is usually a nuisance since the listing is out of order: file_1, file_10, file_11, ..., file_2.
#' This function makes it easy to create file names such as file_01, file_02, ..., file_09,
#' file_10, file_11, ....
#'
#' @param Integer to be converted to string.
#' @param n number of digits to be used.
#' @return String containing the number padded on its left with 0s \code{n} characters long.
#' @examples
#' print(numString(1:10, 2))
#' print(numString(1:10, 4))
#' @export
numString <- function(x, n=3)
  substring(paste(10^n + x), 2)


#' @rdname buildFileName
#' @export
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

#' Sequentially numbered file name creator
#'
#' Useful functions that create sequentially numbered file
#'  names.
#'
#' It is very common useful to build file names with sequential numbers
#' within. So that the numbers appear in the correct order in directory
#' listings or simply that when viewing the file names they appear
#' homogeneous it is nice to have a function that perform build this file
#' names easily. \code{buildFileName} takes as input a sequence of numbers
#' and builds the file names with given prefix, extension and
#' separator.
#' 
#' The other function, \code{buildFileNameFun} returns a
#' function that creates the file name given the indexes. It allows
#' multiple indexes so that, for instance a grid of file names can be
#' created.
#'
#' @param n Numbers used to build file name.
#' @param prefix Prefix of the filename.
#' @param ext File name extension. If \code{NULL}, use no extension.
#' @param sep Separator element between different parts of the file name
#' @param nc Number of characters to use when building the numbers. If null use largest number in \code{n}.
#' @return Character vector with the names.
#' @examples
#' print(buildFileName(1:10, ext='.txt'))
#' print(buildFileName(1:10, ext='.txt', sep='_'))
#' print(buildFileName(1:10, ext=NULL))
#' print(buildFileName(1:10, nc=3))
#' @export
buildFileName <- function(n, prefix='ponto', ext=NULL, sep='-', nc=NULL){
  if (is.null(nc)) nc <- nchar(max(n))
  if (is.null(ext)) ext <- ""
  paste(prefix, sep, numString(n, nc), ext, sep='')
}


#' \code{\link{parse}} with better defaults.
#'
#' This function is now superseeded by \code{\link{paste0}}!
#' 
#'Simply a wrapper around \code{\link{parse}} where the default separator is an empty character.
#' Uses the same parameters as \code{\link{parse}}.
#'
#' @param ... Arguments to be passed to \code{\link{paste}}.
#' @param sep Separator.
#' @param collapse Character that joins vectors.
#' @return A character vector.
#' @seealso \code{\link{paste0}}
#' @examples
#' print(join(1,2,3))
#' print(join(1:10))
#' print(join(1:10, collapse="."))
#' @export
join <- function(..., sep='', collapse=NULL)
  paste(..., sep=sep, collapse=collapse)

#' Improved  \code{\link{basename}}.
#' 
#' Simply a wrapper around \code{\link{basename}} that allows
#' removing the extension part from file names.
#'
#' The R function \code{\link{basename}}^is very useful when writing scripts but often
#' it is necessary to change the extension. The UNIX utililty \code{basename} allows for a
#' second argument the provides a termination suffix that can be excluded.
#'
#' @param path Path name.
#' @param ext Extension to be excluded. If \code{NULL}, just use \code{\link{basename}}.
#' @return Basename of the path.
#' @examples
#' print(basename2("/home/file.txt"))
#' print(basename2("/home/file.txt", ext='.txt'))
#' @export
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


#' A more specific case of \code{\link{rep}}.
#'
#' This function is used to create e vector of repeating elements
#' where the number of times each number is repeated can be
#' specified independently.
#'
#' @param x Vector with elements that should be repeated.
#' @param r Vector of the same size as \code{x} specifiying how many times each element should be repeated.
#' @return Vector with repeated elements.
#' @seealso \code{\link{rep}}
#' @examples
#' print(rep2(c(1,2,3), c(1,2,3)))
#' @export
rep2 <- function(x, r){

  y <- vector()
  nx <- length(x)
  for (i in 1:nx)
    y <- c(y, rep(x[i], r[i]))
  return(y)
}

#' Identity function.
#'
#' A function that simply returns its argument.
#'
#' @param x Argument to be returned.
#' @return The input argument.
#' @export
idfun <- function(x) x

#' Joins elements of a list according to some criteria.
#'
#' This function is a flexible version of rbind, cbind c.
#' Its binds together every elements
#'
#' Often it is necessary to join elements of a list according to some criteria.
#' This function does that in a very flexible way. The binding function can be specified,
#' and a function can be applied to each element before binding. Another feature
#' is the possibility to do this recursively. The function \code{bindArgs} does
#' basically the same thing, except that it binds the variable list arguments instead of
#' the elements of a list.
#'
#' @param lst List whose elements shoud be binded.
#' @param bindfun Bind function that should be used to join the elements.
#' @param fun Function to be applied to each element of the list.
#' @param recursive How many levels of recursrion should be used.
#' @param ... Arguments to be binded in \code{bindArgs}.
#' @seealso \code{\link{rbind}} \code{\link{cbind}} \code{\link{c}} \code{\link{lapply}}
#' @examples
#' x <- matrix(rnorm(12), nr=3, nc=4)
#' lst <- lapply(1:4, function(i) x[,i])
#' print(bindList(lst, cbind) - x)
#' print(bindList(lst, rbind) - t(x))
#' print(bindList(lst, c, mean))
#' lst2 <- list(lst, lst, lst)
#' print(bindList(lst2, rbind, mean, recursive=1))
#' print(bindArgs(lst[[1]], lst[[2]], lst[[3]], lst[[4]], bindfun=cbind) - x)
#' @export
bindList <- function(lst, bindfun=rbind, fun=NULL, recursive=0){
  
  
  if (recursive > 0 || is.null(fun))
    localfun <- idfun
  else
    localfun <- fun
  
  n <- length(lst)
  if (recursive==0){
    x <- localfun(lst[[1]])
    if (n>1)
      for (i in 2:n)
        x <- bindfun(x, localfun(lst[[i]]))
  }else{
    x <- bindList(lapply(lst, bindList, bindfun=bindfun, fun=fun,
                         recursive=recursive-1), bindfun, NULL, 0)
  }
  return(x)
}

#' @rdname bindList
#' @export
bindArgs <- function(..., bindfun=rbind, fun=NULL, recursive=0)
  bindList(list(...), bindfun=bindfun, fun=fun, recursive=recursive)


#' Calculates the hypotenuse of the arguments.
#'
#' Calculates the n dimensional hypotenuse of the arguments. It calculates
#' the Euclidean norm of the arguments.
#'
#' The hypotenuse of a 2D triangle is given by \code{h = sqrt(x^2 + y^2)}.
#' In the n dimensional case, keep adding the terms...
#'
#' @param ... Numeric vectors.
#' @return Numeric vector.
#' @examples
#' print(hypot(3, 4))
#' print(hypot(3,4,12))
#' @export
hypot <- function(...){
  args <- list(...)
  h <- 0
  for (a in args)
    h <- h + abs(a)**2
  return(sqrt(h))
}



#' Creates n-dimensional grids.
#'
#' Creates an n dimensional grid given
#' values in each coordinate direction.
#'
#' Very often, rectangular grid is necessary given
#' the coordinates along each direction resulting
#' in the coordinates of each point in the grid. This
#' function generalizes this such that it works
#' for n-dimensional cubes.
#'
#' @param ... Numeric vectors. The first arguments run faster than later ones.
#' @param use.array Return result as a list of arrays?
#' @param use.df Return result as a data.frame (if \code{arr=FALSE})?
#' @return List with arrays.
#' @examples
#' g <- ndgrid(x=1:3, y=11:14)
#' print(g)
#' h <- ndgrid(x=1:3, y=11:14, z=101:102)
#' print(h)
#' w <- ndgrid(x=1:3, y=11:14, use.array=TRUE)
#' print(w)
#' @export
ndgrid <- function(..., use.array=FALSE, use.df=TRUE){

  lst <- list(...)

  dims <- sapply(lst, length)
  nargs <- length(lst)
  res <- list()
  for (i in 1:nargs){
    if (i == nargs)
      r <- 1
    else
      r <- prod(dims[(i+1):nargs])

    if (i == 1)
      each <- 1
    else
      each <- prod(dims[1:(i-1)])

    x <- rep(lst[[i]], each=each, times=r)

    if (use.array) dim(x) <- dims
    res[[i]] <- x
  }
  names(res) <- names(lst)
  if (!use.array)
    if (use.df)
      res <- data.frame(res)
  return(res)
      
}
