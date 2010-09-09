\name{msgSimNao}
\alias{msgSimNao}
\title{Sim/Não input}
\description{Waits for Sim/Não input from user.}
\usage{
msgSimNao(msg='')
}
\arguments{
  \item{msg}{Prompt to be displayed before asking S/N.}
}
\value{\code{TRUE} if user answered Sim \code{FALSE} otherwise.}
\seealso{
  \code{\link{mymenu}}
}

\examples{
ans <- msgSimNao("Plotar curva?")
if (ans)
  plot(1:10, main="Plotou!!!")
else
  print("Sem curva")

}