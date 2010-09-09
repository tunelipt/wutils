\name{mymenu}
\alias{mymenu}
\title{Simple menu input.}
\description{Implements a simple menu were user can choose one of
  several options.}
\usage{
mymenu(choices, title="", pt=FALSE)
}
\arguments{
\item{choices}{Menu options.}
\item{title}{Title to be displayed in the menu.}
\item{pt}{Use portuguese?}
}
\value{Index of the chosen option.}
\seealso{
  \code{\link{msgYesNo}}
}

\examples{
ans <- mymenu(c("One", "Two", "Three"), "Select an option")
print(ans)
}