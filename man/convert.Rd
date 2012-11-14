\name{convert}
\alias{convert}
\title{Converts an image from a color keymap to grey scale.}
\usage{
  convert(img, tol = 20, clen = 100,
    myrain = col2rgb(cfd.colors(clen)),
    mygray = seq(0.02, 0.96, len = clen))
}
\arguments{
  \item{img}{Array containing the image.}

  \item{tol}{Tolerance in color interpolation.}

  \item{clen}{Number of color subdivisions that should be
  used.}

  \item{myrain}{Color scale.}

  \item{mygray}{Gray scale that is used when generating
  output.}
}
\value{
  Gray scale image.
}
\description{
  An utility function used when converting some figure that
  has a color map from a color scale to a grey scale.
}
\details{
  If a pixel has o color too far away from the color scale,
  the pixel is left without modification since it probably
  belongs to annotations on the figure.
}

