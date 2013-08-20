\name{lst2array}
\alias{lst2array}
\title{Converte uma lista em um array.}
\usage{
  lst2array(xlst)
}
\arguments{
  \item{xlst}{List de arrays com mesma dimensão.}
}
\value{
  Array com dim=c(dim(xlst[[1]]), length(xlst)).
}
\description{
  Em quase todos os ensaios, existem dados que foram
  calculados para diferentes ângulos e estes dados estão
  armazenados em uma lista onde cada elemento corresponde a
  um ângulo. Se os dados tiverem mesmas dimensões em todos
  os ângulos, pode-se juntar tudo em um array de tamanho
  apropriado.
}
\details{
  Se cada elemento da lista que é fornecido como argumento
  (\code{xlst}) tiver a mesma dimensão, esta função cria um
  array para armazenar todos estes dados. A única restrição
  é que as dimensões do array armazenado em \code{xlst}
  sejam as mesmas.
}
\examples{
x <- lapply(1:5, function(i) array(rnorm(8*3*5), dim=c(8,3,5), dimnames=list(NULL, c('FX','FY','FZ'), paste('T', 1:5, sep=''))))
y <- lst2array(x)
}

