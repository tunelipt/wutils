\name{mymenu}
\alias{mymenu}
\title{Simple menu input.}
\usage{
  mymenu(choices, title = "", pt = FALSE)
}
\arguments{
  \item{choices}{Menu options.}

  \item{title}{Title to be displayed in the menu.}

  \item{pt}{Use portuguese on prompt?}
}
\value{
  Index of the chose option.
}
\description{
  Implements a simple menu were user can choose one of
  several options.
}
\examples{
ans <- mymenu(c("One", "Two", "Three"), "Select an option")
print(ans)
}
\seealso{
  \code{\link{msgYesNo}}
}

