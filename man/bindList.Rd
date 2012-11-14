\name{bindList}
\alias{bindArgs}
\alias{bindList}
\title{Joins elements of a list according to some criteria.}
\usage{
  bindList(lst, bindfun = rbind, fun = NULL, recursive = 0)

  bindArgs(..., bindfun = rbind, fun = NULL, recursive = 0)
}
\arguments{
  \item{lst}{List whose elements shoud be binded.}

  \item{bindfun}{Bind function that should be used to join
  the elements.}

  \item{fun}{Function to be applied to each element of the
  list.}

  \item{recursive}{How many levels of recursrion should be
  used.}

  \item{...}{Arguments to be binded in \code{bindArgs}.}
}
\description{
  This function is a flexible version of rbind, cbind c.
  Its binds together every elements
}
\details{
  Often it is necessary to join elements of a list
  according to some criteria. This function does that in a
  very flexible way. The binding function can be specified,
  and a function can be applied to each element before
  binding. Another feature is the possibility to do this
  recursively. The function \code{bindArgs} does basically
  the same thing, except that it binds the variable list
  arguments instead of the elements of a list.
}
\examples{
x <- matrix(rnorm(12), nr=3, nc=4)
lst <- lapply(1:4, function(i) x[,i])
print(bindList(lst, cbind) - x)
print(bindList(lst, rbind) - t(x))
print(bindList(lst, c, mean))
lst2 <- list(lst, lst, lst)
print(bindList(lst2, rbind, mean, recursive=1))
print(bindArgs(lst[[1]], lst[[2]], lst[[3]], lst[[4]], bindfun=cbind) - x)
}

