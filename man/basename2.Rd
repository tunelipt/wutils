\name{basename2}
\alias{basename2}
\title{Improved \code{\link{basename}}}
\description{Simply a wrapper around \code{\link{basename}} that allows
  removing the extension part from file names.}
\usage{
basename2(path, ext=NULL)
}
\arguments{
  \item{path}{Path name}
  \item{ext}{Whether to remove the extensio}
}
\value{The basename of the path}

\examples{

print(basename2("/home/file.txt"))
print(basename2("/home/file.txt", ext='.txt'))

}