\name{mymenu}
\alias{mymenu}
\title{Simple menu input.}
\description{Implements a simple menu were user can choose one of
  several options.}
\usage{
mymenu(choices, title="")
}
\arguments{
\item{choices}{Menu options.}
\item{title}{Title to be displayed in the menu.}
}
\value{Index of the chosen option.}
\seealso{
  \code{\link{msgSimNao}}
}

\examples{
ans <- mymenu(c("Um", "Dois", "Três"), "Escolha um numero")
print(ans)
}