\name{msgYesNo}
\alias{msgYesNo}
\title{Y/N (or S/N)  input}
\description{Waits for a yes or no  input from user, allowing for
  portuguese options.}
\usage{
msgYesNo(msg='', pt=FALSE)
}
\arguments{
  \item{msg}{Prompt to be displayed before asking Y/N.}
  \item{pt}{Whether to use portuguese as language.}
}
\value{\code{TRUE} if user answered Sim \code{FALSE} otherwise.}
\seealso{
  \code{\link{mymenu}}
}

\examples{
ans <- msgYesNo("Sim ou Não", pt=TRUE)
print(ans)
ans <- msgYesNo("Yes or No")
print(ans)
}