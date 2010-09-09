\name{buildFileName}
\alias{buildFileName}
\alias{buildFileNameFun}

\title{Sequentially numbered file name creator}
\description{Useful functions that create sequentially numbered file
  names.}

\usage{
buildFileNameFun(..., prefix="ponto", ext=".rda", sep='-')
buildFileName(n, prefix='ponto', ext='.rda', sep='-', nc=NULL)
}
\arguments{
\item{...}{Sequence of numbers for multi-numbered file names.}
\item{prefix}{Prefix of the filename.}
\item{ext}{File name extension}
\item{sep}{Separator element between different parts of the file name}
\item{n}{Numbers used to build file name.}
\item{nc}{Number of characters to use when building the numbers.}
}
\details{
It is very common useful to build file names with sequential numbers
within. So that the numbers appear in the correct order in directory
listings or simply that when viewing the file names they appear
homogeneous it is nice to have a function that perform build this file
names easily. \code{buildFileName} takes as input a sequence of numbers
and builds the file names with given prefix, extension and
separator.

The other function, \code{buildFileNameFun} returns a
function that creates the file name given the indexes. It allows
multiple indexes so that, for instance a grid of file names can be
created.

}

\examples{

print(buildFileName(1:10, nc=3))
f <- buildFileNameFun(100, 100)
print(f(1, 1))
print(f(10, 20))
}