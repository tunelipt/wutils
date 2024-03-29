% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{msgYesNo}
\alias{msgYesNo}
\title{Displays a Yes/No prompt.}
\usage{
msgYesNo(msg = "", pt = FALSE)
}
\arguments{
\item{pt}{Use portuguese?}

\item{msh}{Prompt to be displayed before asking Y/N.}
}
\value{
TRUE (YES), FALSE (NO).
}
\description{
Simple function for interactive input.
This function displays a prompt that await a Yes/No reply.
If the optional argument \code{pt} is TRUE, the function
expects S /N (portuguese).
}
\examples{
ans <- msgYesNo("Sim ou Não", pt=TRUE)
print(ans)
ans <- msgYesNo("Yes or No")
print(ans)
}
