\name{readInteger}
\alias{readInteger}
\alias{readNumber}
\title{Integer/number input function}
\description{Reads an integer/number from the prompt}
\usage{
readInteger(msg='', pt=FALSE)
readNumber(msg='', pt=FALSE)
}
\arguments{
  \item{msg}{Prompt to be displayed}
  \item{pt}{Use portuguese messages?}
}
\details{
  These functions print a prompt and repeats until the correct type of
  input is read and returns the input. The comments and messages can be
  displayed in portuguese if option \code{pt} is set to \code{TRUE}.

  \code{readInteger} expects an integer from the user and
  \code{readNumber} a number (float or integer).
}
\value{The number/integer read.}

\examples{

print(readInteger("Enter an integer: "))
print(readNumber("Enter a number: "))
print(readInteger("Entre com um inteiro: ", pt=TRUE))
print(readNumber("Entre com um numero: ", pt=FALSE))


}